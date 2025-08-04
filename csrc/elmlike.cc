#include "elmlike.h"

#include <functional>
#include <cstdio>
#include <cassert>
#include <thread>
#include <unistd.h>
#include <queue>
#include <mutex>
#include <memory>

#include "VkBootstrap.h"

#define GLFW_INCLUDE_VULKAN
#include "GLFW/glfw3.h"

#include <vulkan/vulkan.h>

namespace {

enum class EventSignal: int {
  NONE = 0,
  QUIT,
};
std::queue<EventSignal> _event_queue;
std::mutex _event_queue_mutex;

// TODO: Do not keep these variables globally allocated...
struct Renderer {
  GLFWwindow *window = nullptr;
  vkb::Device vk_device;
  VkSurfaceKHR vk_surface;
  vkb::Instance vk_instance;

  ~Renderer();
};

std::thread _hs_thread;
bool _start_gui = false;

void run_ui_loop(Renderer& renderer) {
  assert(renderer.window);
  glfwMakeContextCurrent(renderer.window);
  while (!glfwWindowShouldClose(renderer.window)) {
    glfwSwapBuffers(renderer.window);
    glfwPollEvents();
  }
  {
    std::scoped_lock l(_event_queue_mutex);
    _event_queue.push(EventSignal::QUIT);
  }
  printf("Window closed.\n");
}

int init_window_with_skia(Renderer& renderer) {
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

  if (renderer.window != nullptr) {
    fprintf(stderr, "Window already initialized.\n");
    return 1;
  }
  renderer.window = glfwCreateWindow(800, 600, "Elmlike",
    nullptr, nullptr);
  if (renderer.window == nullptr) {
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
  if (glfwCreateWindowSurface(instance_ret.value(), renderer.window, nullptr, &surface)) {
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

  renderer.vk_device = device_ret.value();
  renderer.vk_instance = instance_ret.value();
  renderer.vk_surface = surface;
  return 0;
}

Renderer::~Renderer() {
  if (!this->window) {
    return;
  }

  vkb::destroy_device(this->vk_device);
  vkb::destroy_surface(this->vk_instance, this->vk_surface);
  vkb::destroy_instance(this->vk_instance);

  printf("Stopping GUI.\n");
  glfwDestroyWindow(this->window);
  glfwTerminate();
}

} // namespace

void start_gui() {
  _start_gui = true;
}

EventSignal _poll_event_signal() {
  usleep(100'000);
  std::scoped_lock l(_event_queue_mutex);
  if (_event_queue.empty()) {
    return EventSignal::NONE;
  }
  const EventSignal sig = _event_queue.front();
  _event_queue.pop();
  return sig;
}

int poll_event_signal() {
  return static_cast<int>(_poll_event_signal());
}

void UiExec(std::function<void()> hs_entry) {
  printf("[UiExec] starting hs thread.\n");
  _hs_thread = std::thread(hs_entry);
  // TODO: Remove this sleep lol.
  while (!_start_gui) {
    usleep(100'000); // 100ms
  }
  printf("[UiExec] received signal to start ui thread.\n");

  std::unique_ptr<Renderer> renderer = std::make_unique<Renderer>();
  init_window_with_skia(*renderer);
  run_ui_loop(*renderer);
  _hs_thread.join();
  printf("[UiExec] stopping hs thread.\n");
}
