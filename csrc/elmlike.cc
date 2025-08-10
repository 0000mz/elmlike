#include "elmlike.h"

#include <dlfcn.h>

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <functional>
#include <include/core/SkColorType.h>
#include <memory>
#include <mutex>
#include <queue>
#include <span>
#include <string_view>
#include <thread>
#include <unistd.h>
#include <utility>
#include <vector>

#include "VkBootstrap.h"

#define GLFW_INCLUDE_VULKAN
#include "GLFW/glfw3.h"

#include <vulkan/vulkan.h>
#include <vulkan/vulkan_core.h>

#include "skia/include/core/SkCanvas.h"
#include "skia/include/core/SkColor.h"
#include "skia/include/core/SkImage.h"
#include "skia/include/core/SkImageInfo.h"
#include "skia/include/core/SkPaint.h"
#include "skia/include/core/SkRect.h"
#include "skia/include/core/SkColorSpace.h"
#include "skia/include/core/SkRefCnt.h"
#include "skia/include/core/SkSurface.h"
#include "skia/include/gpu/vk/VulkanBackendContext.h"
#include "skia/include/gpu/ganesh/GrTypes.h"
#include "skia/include/gpu/vk/VulkanTypes.h"
#include "skia/include/gpu/ganesh/vk/GrVkTypes.h"
#include "skia/include/gpu/ganesh/vk/GrVkBackendSurface.h"
#include "skia/include/gpu/ganesh/GrBackendSurface.h"
#include "skia/include/gpu/ganesh/GrDirectContext.h"
#include "skia/include/gpu/ganesh/SkSurfaceGanesh.h"
#include "skia/include/gpu/ganesh/vk/GrVkDirectContext.h"

namespace {

constexpr int kVulkanVersionMajor = 1;
constexpr int kVulkanVersionMinor = 2;
constexpr uint32_t kVulkanApiVersionMacroValue = VK_API_VERSION_1_2;
const char *kVulkanVkMoltenSharedObjectName = "libMoltenVK.dylib";
const char *kVulkanSharedObjectName = "libvulkan.1.dylib";

enum class EventSignal : int {
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
  vkb::Swapchain swapchain;

  std::vector<sk_sp<SkSurface>> skia_surfaces;
  std::vector<VkImage> swapchain_images;
  std::vector<VkImageView> swapchain_image_views;
  std::vector<VkFence> fences;

  std::vector<VkSemaphore> image_available_semaphores;
  std::vector<VkSemaphore> render_finished_semaphores;

  VkCommandPool command_pool;
  VkCommandBuffer command_buffer;

  // Skia resources
  sk_sp<GrDirectContext> skia_ctx;

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

void run_ui_loop(Renderer &renderer) {
  assert(renderer.window);
  glfwMakeContextCurrent(renderer.window);

  uint32_t current_frame = 0;
  while (!glfwWindowShouldClose(renderer.window)) {
    glfwPollEvents();

    vkWaitForFences(renderer.vk_device, 1, &renderer.fences[current_frame], VK_TRUE,
                    UINT64_MAX);
    vkResetFences(renderer.vk_device, 1, &renderer.fences[current_frame]);
    vkQueueWaitIdle(renderer.graphics_queue);
    vkResetCommandBuffer(renderer.command_buffer, 0);

    uint32_t image_index;
    VkResult result = vkAcquireNextImageKHR(
        renderer.vk_device, renderer.swapchain, UINT64_MAX,
        renderer.image_available_semaphores[current_frame], VK_NULL_HANDLE, &image_index);

    if (result == VK_ERROR_OUT_OF_DATE_KHR) {
      // TODO: Handle swapchain recreation
      continue;
    } else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR) {
      fprintf(stderr, "Failed to acquire swapchain image.\n");
      break;
    }

    SkCanvas *canvas = renderer.skia_surfaces[image_index]->getCanvas();

    { // Test draw
      SkPaint paint;
      paint.setColor(SK_ColorBLUE);
      paint.setStyle(SkPaint::kFill_Style);
      paint.setAntiAlias(true);

      SkRect rect = SkRect::MakeXYWH(50, 50, 100, 100);

      canvas->drawRect(rect, paint);
      renderer.skia_ctx->flushAndSubmit();
    }

    // Begin recording to the command buffer
    VkCommandBufferBeginInfo begin_info{};
    begin_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
    begin_info.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;

    vkBeginCommandBuffer(renderer.command_buffer, &begin_info);

    // Image memory barrier to transition layout for presentation
    VkImageMemoryBarrier image_barrier{};
    image_barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
    image_barrier.oldLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
    image_barrier.newLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
    image_barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
    image_barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
    image_barrier.image = renderer.swapchain_images[image_index];
    image_barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
    image_barrier.subresourceRange.baseMipLevel = 0;
    image_barrier.subresourceRange.levelCount = 1;
    image_barrier.subresourceRange.baseArrayLayer = 0;
    image_barrier.subresourceRange.layerCount = 1;
    image_barrier.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
    image_barrier.dstAccessMask = VK_ACCESS_MEMORY_READ_BIT;

    vkCmdPipelineBarrier(renderer.command_buffer,
                         VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                         VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, 0, 0, nullptr, 0,
                         nullptr, 1, &image_barrier);

    vkEndCommandBuffer(renderer.command_buffer);

    VkSubmitInfo submit_info{};
    submit_info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;

    VkSemaphore wait_semaphores[] = {renderer.image_available_semaphores[current_frame]};
    VkPipelineStageFlags wait_stages[] = {
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
    submit_info.waitSemaphoreCount = 1;
    submit_info.pWaitSemaphores = wait_semaphores;
    submit_info.pWaitDstStageMask = wait_stages;
    submit_info.commandBufferCount = 1; // Now submitting one command buffer
    submit_info.pCommandBuffers = &renderer.command_buffer; // Our command buffer

    VkSemaphore signal_semaphores[] = {renderer.render_finished_semaphores[current_frame]};
    submit_info.signalSemaphoreCount = 1;
    submit_info.pSignalSemaphores = signal_semaphores;

    if (vkQueueSubmit(renderer.graphics_queue, 1, &submit_info,
                      renderer.fences[current_frame]) != VK_SUCCESS) {
      fprintf(stderr, "Failed to submit draw command buffer.\n");
      break;
    }

    VkPresentInfoKHR present_info{};
    present_info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
    present_info.waitSemaphoreCount = 1;
    present_info.pWaitSemaphores = signal_semaphores;

    VkSwapchainKHR swapchains[] = {renderer.swapchain};
    present_info.swapchainCount = 1;
    present_info.pSwapchains = swapchains;
    present_info.pImageIndices = &image_index;

    result = vkQueuePresentKHR(renderer.graphics_queue, &present_info);
    if (result == VK_ERROR_OUT_OF_DATE_KHR || result == VK_SUBOPTIMAL_KHR) {
      // TODO: Handle swapchain recreation
    } else if (result != VK_SUCCESS) {
      fprintf(stderr, "Failed to present swapchain image.\n");
      break;
    }

    printf("Submitted frame idx %u\n", current_frame); sleep(3);
    current_frame = (current_frame + 1) % renderer.swapchain.image_count;
  }
  signal_hs_shutdown();
  printf("Window closed.\n");
}

PFN_vkVoidFunction VulkanGetProcAddr(const char *function_name,
                                     VkInstance instance, VkDevice device) {
  const PFN_vkVoidFunction fn1 = vkGetInstanceProcAddr(instance, function_name);
  const PFN_vkVoidFunction fn2 = vkGetDeviceProcAddr(device, function_name);
  // if (fn1 || fn2) printf("[vkProcAddr] fn: %s fn1: %p fn2: %p\n",
  // function_name, fn1, fn2);
  if (fn1)
    return fn1;
  if (fn2)
    return fn2;

#if __APPLE__
  static void *vulkan_lib_handle =
      dlopen(kVulkanVkMoltenSharedObjectName, RTLD_LAZY);
#else
  static void *vulkan_lib_handle = dlopen(kVulkanSharedObjectName, RTLD_LAZY);
#endif
  if (!vulkan_lib_handle) {
    fprintf(stderr, "Failed to load libvulkan.\n");
    return nullptr;
  }
  void *fn3 = dlsym(vulkan_lib_handle, function_name);
  // printf("[vkProcAddr] fn: %s fn1: %p fn2: %p fn3: %p\n", function_name, fn1,
  // fn2, fn3);
  return (PFN_vkVoidFunction)fn3;
}

int SetupSkia(Renderer &renderer) {
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

  // Get the swapchain images and image views
  renderer.swapchain_images = renderer.swapchain.get_images().value();
  renderer.swapchain_image_views =
      renderer.swapchain.get_image_views().value();

  // Create a Skia surface for each swapchain image
  renderer.skia_surfaces.resize(renderer.swapchain.image_count);
  for (uint32_t i = 0; i < renderer.swapchain.image_count; ++i) {
    const uint32_t width = renderer.swapchain.extent.width;
    const uint32_t height = renderer.swapchain.extent.height;

    GrVkImageInfo vk_image_info;
    vk_image_info.fImage = renderer.swapchain_images.at(i);
    vk_image_info.fFormat = renderer.swapchain.image_format;
    vk_image_info.fLevelCount = 1;
    vk_image_info.fSampleCount = 1;

    GrBackendTexture tex = GrBackendTextures::MakeVk(static_cast<int>(width),
        static_cast<int>(height), vk_image_info);

    const SkImageInfo image_info = SkImageInfo::Make(
        static_cast<int>(width), static_cast<int>(height),
        kBGRA_8888_SkColorType,
        kPremul_SkAlphaType, nullptr);

    sk_sp<SkSurface> surface = SkSurfaces::WrapBackendTexture(
        skia_ctx.get(), tex,
        GrSurfaceOrigin::kTopLeft_GrSurfaceOrigin,
        1, // samples-per-pixel
        image_info.colorInfo().colorType(),
        nullptr, // color-space: todo, set
        nullptr, nullptr);

    if (!surface) {
      fprintf(stderr, "Failed to create skia surface for swapchain image.\n");
      return 1;
    }
    renderer.skia_surfaces[i] = surface;
  }

  renderer.skia_ctx = std::move(skia_ctx);
  printf("Successfully initialized skia.\n");

  return 0;
}


int init_window_with_skia(Renderer &renderer) {
  printf("Starting GUI.\n");

  constexpr uint32_t kWidth = 800;
  constexpr uint32_t kHeight = 600;

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
  renderer.window =
      glfwCreateWindow(kWidth, kHeight, "Elmlike", nullptr, nullptr);
  if (renderer.window == nullptr) {
    fprintf(stderr, "Failed to create window.\n");
    return 1;
  }

  /// Vulkan Extensions Setup
  vkb::SystemInfo system_info = vkb::SystemInfo::get_system_info().value();
  printf("Available Instance Extensions: \n");
  for (const auto &extension : system_info.available_extensions) {
    printf("\t>%s\n", extension.extensionName);
  }

  uint32_t glfw_extension_count = 0;
  const char **glfw_extensions;
  glfw_extensions = glfwGetRequiredInstanceExtensions(&glfw_extension_count);

  std::span<const char *> glfw_extensions_span(glfw_extensions,
                                               glfw_extension_count);
  std::vector<const char *> extensions(glfw_extensions_span.begin(),
                                       glfw_extensions_span.end());
  printf("Setting up vulkan w/ %ld extensions:\n", extensions.size());
  for (const std::string_view ext_name : extensions) {
    printf("\t> %s\n", std::string(ext_name).c_str());
  }

  /// Vulkan Setup
  /// 1. Creating the VkInstance.
  vkb::InstanceBuilder builder;
  auto instance_ret =
      builder.set_app_name("Elmlike Vk Renderer")
          .request_validation_layers(true)
          .use_default_debug_messenger()
          .enable_extensions(extensions.size(), extensions.data())
          .set_minimum_instance_version(kVulkanVersionMajor,
                                        kVulkanVersionMinor)
          .require_api_version(kVulkanVersionMajor, kVulkanVersionMinor)
          .build();
  if (!instance_ret.has_value()) {
    fprintf(stderr, "vkbootstrap: failed to build instance: %s\n",
            instance_ret.error().message().c_str());
    return 1;
  }

  /// 2. Creating the VkSurface
  VkSurfaceKHR surface = VK_NULL_HANDLE;
  if (glfwCreateWindowSurface(instance_ret.value(), renderer.window, nullptr,
                              &surface)) {
    fprintf(stderr, "Failed to create vulkan surface.\n");
    return 1;
  }

  /// 3. Select physical device.
  const std::vector<const char *> physical_device_extensions{
      "VK_KHR_get_memory_requirements2",
  };

  vkb::PhysicalDeviceSelector selector{instance_ret.value()};
  auto phys_ret =
      selector.set_surface(surface)
          .set_minimum_version(kVulkanVersionMajor, kVulkanVersionMinor)
          .add_required_extensions(physical_device_extensions)
          .select();
  if (!phys_ret.has_value()) {
    fprintf(stderr, "Failed to select vulkan physical device:%s\n",
            phys_ret.error().message().c_str());
    return 1;
  }

  /// 4. Select vulkan device
  vkb::DeviceBuilder device_builder{phys_ret.value()};
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

  vkb::SwapchainBuilder swapchain_builder{renderer.vk_device};
  auto swapchain_ret = swapchain_builder
    .set_desired_extent(kWidth, kHeight)
    .add_image_usage_flags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT)
    .set_desired_format({
      .format = VK_FORMAT_R8G8B8A8_SRGB,
      .colorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
    })
    .build();
  if (!swapchain_ret) {
    fprintf(stderr, "Failed to create swapchain: %s\n",
            swapchain_ret.error().message().c_str());
    return 1;
  }
  renderer.swapchain = swapchain_ret.value();

  // Create synchronization primitives
  renderer.image_available_semaphores.resize(renderer.swapchain.image_count);
  renderer.render_finished_semaphores.resize(renderer.swapchain.image_count);

  VkSemaphoreCreateInfo semaphore_info{};
  semaphore_info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
  for (size_t i = 0; i < renderer.swapchain.image_count; i++) {
    if (vkCreateSemaphore(renderer.vk_device, &semaphore_info, nullptr,
                          &renderer.image_available_semaphores[i]) != VK_SUCCESS ||
        vkCreateSemaphore(renderer.vk_device, &semaphore_info, nullptr,
                          &renderer.render_finished_semaphores[i]) != VK_SUCCESS) {
      fprintf(stderr, "Failed to create semaphores for a frame.\n");
      return 1;
    }
  }

  VkFenceCreateInfo fence_info{};
  fence_info.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
  fence_info.flags = VK_FENCE_CREATE_SIGNALED_BIT;
  renderer.fences.resize(renderer.swapchain.image_count);
  for (auto &fence : renderer.fences) {
    if (vkCreateFence(renderer.vk_device, &fence_info, nullptr, &fence) !=
        VK_SUCCESS) {
      fprintf(stderr, "Failed to create fences.\n");
      return 1;
    }
  }


  // Create command pool
  const std::vector<VkQueueFamilyProperties> queue_families = renderer.vk_physical_device.get_queue_families();
  auto graphics_queue_itr = std::find_if(queue_families.begin(), queue_families.end(), [](const VkQueueFamilyProperties& qfprop) {
    return qfprop.queueFlags & VK_QUEUE_GRAPHICS_BIT;
  });
  if (graphics_queue_itr == queue_families.end()) {
    fprintf(stderr, "Could not find graphics queue family on physical device.\n");
    return 1;
  }
  VkCommandPoolCreateInfo pool_info{};
  pool_info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  pool_info.queueFamilyIndex = static_cast<uint32_t>(std::distance(queue_families.begin(), graphics_queue_itr));
  pool_info.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;

  if (vkCreateCommandPool(renderer.vk_device, &pool_info, nullptr,
                          &renderer.command_pool) != VK_SUCCESS) {
    fprintf(stderr, "Failed to create command pool.\n");
    return 1;
  }

  // Allocate command buffer
  VkCommandBufferAllocateInfo alloc_info{};
  alloc_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  alloc_info.commandPool = renderer.command_pool;
  alloc_info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  alloc_info.commandBufferCount = 1;

  if (vkAllocateCommandBuffers(renderer.vk_device, &alloc_info,
                               &renderer.command_buffer) != VK_SUCCESS) {
    fprintf(stderr, "Failed to allocate command buffers.\n");
    return 1;
  }

  return SetupSkia(renderer);
}

Renderer::~Renderer() {
  if (!this->window) {
    return;
  }

  vkDeviceWaitIdle(this->vk_device);

  for (auto &semaphore : this->render_finished_semaphores) {
    vkDestroySemaphore(this->vk_device, semaphore, nullptr);
  }
  for (auto &semaphore : this->image_available_semaphores) {
    vkDestroySemaphore(this->vk_device, semaphore, nullptr);
  }

  for (auto &fence : this->fences) {
    vkDestroyFence(this->vk_device, fence, nullptr);
  }

  this->skia_surfaces.clear();
  this->skia_ctx.reset();

  vkDestroyCommandPool(this->vk_device, this->command_pool, nullptr);

  for (auto &image_view : this->swapchain_image_views) {
    vkDestroyImageView(this->vk_device, image_view, nullptr);
  }

  vkb::destroy_swapchain(this->swapchain);
  vkb::destroy_device(this->vk_device);
  vkb::destroy_surface(this->vk_instance, this->vk_surface);
  vkb::destroy_instance(this->vk_instance);

  printf("Stopping GUI.\n");
  glfwDestroyWindow(this->window);
  glfwTerminate();
}

} // namespace

void start_gui() { _start_gui = true; }

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

int poll_event_signal() { return static_cast<int>(_poll_event_signal()); }

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
