#include "elmlike.h"

#include <cstdio>
#include <cassert>
#include <thread>

#include "VkBootstrap.h"

#define GLFW_INCLUDE_VULKAN
#include "GLFW/glfw3.h"

#include <vulkan/vulkan.h>


namespace {

// TODO: Do not keep these variables globally allocated...
struct Renderer {
  GLFWwindow *window = nullptr;
  vkb::Device vk_device;
  VkSurfaceKHR vk_surface;
  vkb::Instance vk_instance;
};
Renderer _renderer;
std::thread _ui_thread;

void ui_thread_start() {
  // TODO: This crashes on MacOS because MacOS requires rendering
  // to be done on the main thread.
  //
  // > 'nextEventMatchingMask should only be called from the Main
  // > Thread!
  //
  // Need to have a mechanism to have the UI operate on the main
  // thread and the elmlike runtime executing on a separate thread.
  printf("Starting ui thread.\n");

  assert(_renderer.window);
  glfwMakeContextCurrent(_renderer.window);
  while (!glfwWindowShouldClose(_renderer.window)) {
    glfwSwapBuffers(_renderer.window);
    glfwPollEvents();
  }
}

int init_window_with_skia() {
  printf("Starting GUI.\n");

  if (!glfwInit()) {
    fprintf(stderr, "Failed to initialize glfw.\n");
    return 1;
  }

  // Need to set no-api to use Vulkan renderer for glfw
  // since it defaults to creating an OpenGL renderer.
  glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

  if (!glfwVulkanSupported()) {
    fprintf(stderr, "glfw: vulkan not supported.\n");
    return 1;
  }

  if (_renderer.window != nullptr) {
    fprintf(stderr, "Window already initialized.\n");
    return 1;
  }
  _renderer.window = glfwCreateWindow(800, 600, "Elmlike",
    nullptr, nullptr);
  if (_renderer.window == nullptr) {
    fprintf(stderr, "Failed to create window.\n");
    return 1;
  }

  /// Vulkan Setup
  /// 1. Creating the VkInstance.
  vkb::InstanceBuilder builder;
  auto instance_ret = builder.set_app_name("Elmlike Vk Renderer")
    .request_validation_layers(true)
    .use_default_debug_messenger()
    .build();
  if (!instance_ret.has_value()) {
    fprintf(stderr, "vkbootstrap: failed to build instance: %s\n",
      instance_ret.error().message().c_str());
    return 1;
  }

  /// 2. Creating the VkSurface
  VkSurfaceKHR surface = VK_NULL_HANDLE;
  if (glfwCreateWindowSurface(instance_ret.value(), _renderer.window, nullptr, &surface)) {
    fprintf(stderr, "Failed to create vulkan surface.\n");
    return 1;
  }

  /// 3. Select physical device.
  vkb::PhysicalDeviceSelector selector { instance_ret.value() };
  auto phys_ret = selector.set_surface(surface)
    .set_minimum_version(1, 1)
    .select();
  if (!phys_ret.has_value()) {
    fprintf(stderr, "Failed to select vulkan physical device:%s\n",
      phys_ret.error().message().c_str());
    return 1;
  }

  /// 4. Select vulkan device
  vkb::DeviceBuilder device_builder { phys_ret.value() };
  auto device_ret = device_builder.build();
  if (!device_ret.has_value()) {
    fprintf(stderr, "Failed to create vulkan device: %s\n",
      device_ret.error().message().c_str());
    return 1;
  }

  _renderer.vk_device = device_ret.value();
  _renderer.vk_instance = instance_ret.value();
  _renderer.vk_surface = surface;

  _ui_thread = std::thread(ui_thread_start);
  return 0;
}

} // namespace

void start_gui() {
  if (init_window_with_skia() != 0) {
    assert(false);
  }
}

void stop_gui() {
  // TODO: Signal to the ui thread to terminate...
  if (!_renderer.window) {
    return;
  }

  vkb::destroy_device(_renderer.vk_device);
  vkb::destroy_surface(_renderer.vk_instance, _renderer.vk_surface);
  vkb::destroy_instance(_renderer.vk_instance);

  printf("Stopping GUI.\n");
  glfwDestroyWindow(_renderer.window);
  glfwTerminate();
}
