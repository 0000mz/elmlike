#include "elmlike.h"

#include <dlfcn.h>

#include <vector>
#include <cstdint>
#include <span>
#include <string_view>
#include <utility>
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

#include <vulkan/vulkan_core.h>
#include <vulkan/vulkan.h>

#include "skia/include/gpu/ganesh/GrDirectContext.h"
#include "skia/include/gpu/ganesh/vk/GrVkDirectContext.h"
#include "skia/include/gpu/vk/VulkanBackendContext.h"
#include "skia/include/gpu/ganesh/SkSurfaceGanesh.h"
#include "skia/include/core/SkImage.h"
#include "skia/include/core/SkRefCnt.h"
#include "skia/include/core/SkImageInfo.h"
#include "skia/include/core/SkSurface.h"
#include "skia/include/gpu/vk/VulkanTypes.h"
#include "skia/include/gpu/GpuTypes.h"

namespace {

constexpr int kVulkanVersionMajor = 1;
constexpr int kVulkanVersionMinor = 2;
constexpr uint32_t kVulkanApiVersionMacroValue = VK_API_VERSION_1_2;
const char *kVulkanSharedObjectName = "libvulkan.1.dylib";

enum class EventSignal: int {
  NONE = 0,
  QUIT,
};

struct Renderer {
  GLFWwindow *window = nullptr;
  vkb::Device vk_device;
  vkb::PhysicalDevice vk_physical_device;
  VkSurfaceKHR vk_surface;
  vkb::Instance vk_instance;
  VkQueue graphics_queue;

  // Skia resources
  sk_sp<GrDirectContext> skia_ctx;
  sk_sp<SkSurface> skia_surface;

  ~Renderer();
};

// TODO: Do not keep these variables globally allocated...
// {
std::queue<EventSignal> _event_queue;
std::mutex _event_queue_mutex;
std::thread _hs_thread;
bool _start_gui = false;
// }

void signal_hs_shutdown() {
  std::scoped_lock l(_event_queue_mutex);
  _event_queue.push(EventSignal::QUIT);
}

void run_ui_loop(Renderer& renderer) {
  assert(renderer.window);
  glfwMakeContextCurrent(renderer.window);
  while (!glfwWindowShouldClose(renderer.window)) {
    glfwSwapBuffers(renderer.window);
    glfwPollEvents();
  }
  signal_hs_shutdown();
  printf("Window closed.\n");
}

PFN_vkVoidFunction VulkanGetProcAddr(const char* function_name,
    VkInstance instance, VkDevice device) {
  const PFN_vkVoidFunction fn1 = vkGetInstanceProcAddr(instance, function_name);
  const PFN_vkVoidFunction fn2 = vkGetDeviceProcAddr(device, function_name);
  if (fn1 || fn2) printf("[vkProcAddr] fn: %s fn1: %p fn2: %p\n", function_name, fn1, fn2);
  if (fn1) return fn1;
  if (fn2) return fn2;

  static void* handle = dlopen(kVulkanSharedObjectName, RTLD_LAZY);
  if (!handle) {
    fprintf(stderr, "Failed to load libvulkan.\n");
    return nullptr;
  }
  void *fn3 = dlsym(handle, function_name);
  printf("[vkProcAddr] fn: %s fn1: %p fn2: %p fn3: %p\n", function_name, fn1, fn2, fn3);
  return (PFN_vkVoidFunction)fn3;
}

int SetupSkia(Renderer& renderer, const int width, const int height) {
  printf("Setting up skia.\n");

  skgpu::VulkanBackendContext bctx;
  bctx.fInstance = renderer.vk_instance;
  bctx.fPhysicalDevice = renderer.vk_physical_device;
  bctx.fDevice = renderer.vk_device;
  bctx.fQueue = renderer.graphics_queue;
  bctx.fGraphicsQueueIndex = 0;
  bctx.fMaxAPIVersion = kVulkanApiVersionMacroValue;
  bctx.fGetProc = VulkanGetProcAddr;

  sk_sp<GrDirectContext> skia_ctx = GrDirectContexts::MakeVulkan(bctx);
  if (!skia_ctx) {
    fprintf(stderr, "Failed to create skia context.\n");
    return 1;
  }

  SkImageInfo info = SkImageInfo::MakeN32Premul(width, height);
  sk_sp<SkSurface> surface = SkSurfaces::RenderTarget(
    skia_ctx.get(),
    skgpu::Budgeted::kYes,
    info);
  if (!surface) {
    fprintf(stderr, "Failed to create skia surface.\n");
    return 1;
  }

  renderer.skia_ctx = std::move(skia_ctx);
  renderer.skia_surface = std::move(surface);
  printf("Successfully initialized skia.\n");

  return 0;
}

int init_window_with_skia(Renderer& renderer) {
  printf("Starting GUI.\n");

  constexpr int kWidth = 800;
  constexpr int kHeight = 600;

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
  renderer.window = glfwCreateWindow(kWidth, kHeight, "Elmlike",
    nullptr, nullptr);
  if (renderer.window == nullptr) {
    fprintf(stderr, "Failed to create window.\n");
    return 1;
  }

  /// Vulkan Extensions Setup
  vkb::SystemInfo system_info = vkb::SystemInfo::get_system_info().value();
  printf("Available Instance Extensions: \n");
  for (const auto& extension : system_info.available_extensions) {
    printf("\t>%s\n", extension.extensionName);
  }

  uint32_t glfw_extension_count = 0;
  const char** glfw_extensions;
  glfw_extensions = glfwGetRequiredInstanceExtensions(&glfw_extension_count);

  std::span<const char *> glfw_extensions_span(glfw_extensions, glfw_extension_count);
  std::vector<const char *> extensions(glfw_extensions_span.begin(), glfw_extensions_span.end());
  printf("Setting up vulkan w/ %ld extensions:\n", extensions.size());
  for (const std::string_view ext_name : extensions) {
    printf("\t> %s\n", std::string(ext_name).c_str());
  }

  /// Vulkan Setup
  /// 1. Creating the VkInstance.
  vkb::InstanceBuilder builder;
  auto instance_ret = builder.set_app_name("Elmlike Vk Renderer")
    .request_validation_layers(true)
    .use_default_debug_messenger()
    .enable_extensions(extensions.size(), extensions.data())
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
  const std::vector<const char *> physical_device_extensions {
    "VK_KHR_get_memory_requirements2",
  };

  vkb::PhysicalDeviceSelector selector { instance_ret.value() };
  auto phys_ret = selector.set_surface(surface)
    .set_minimum_version(kVulkanVersionMajor, kVulkanVersionMinor)
    .add_required_extensions(physical_device_extensions)
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

  auto graphics_queue_ret = device_ret->get_queue(vkb::QueueType::graphics);
  if (!graphics_queue_ret) {
    fprintf(stderr, "Failed to create vulkan graphics queue: %s\n",
      graphics_queue_ret.error().message().c_str());
    return 1;
  }

  renderer.vk_device = device_ret.value();
  renderer.vk_instance = instance_ret.value();
  renderer.vk_surface = surface;
  renderer.vk_physical_device = phys_ret.value();
  renderer.graphics_queue = graphics_queue_ret.value();
  return SetupSkia(renderer, kWidth, kHeight);
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
  if (init_window_with_skia(*renderer) != 0) {
    fprintf(stderr, "Failed to initialize the gui.\n");
    signal_hs_shutdown();
    _hs_thread.join();
    return;
  }
  run_ui_loop(*renderer);
  _hs_thread.join();
  printf("[UiExec] stopping hs thread.\n");
}
