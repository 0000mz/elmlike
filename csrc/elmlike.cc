#include "elmlike.h"

#include <dlfcn.h>

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <functional>
#include <memory>
#include <mutex>
#include <queue>
#include <thread>
#include <unistd.h>

#include "renderer.h"

#include "absl/base/thread_annotations.h"
#include "absl/synchronization/mutex.h"

namespace {

using ::elmlike::TextNode;
using ::elmlike::UiNode;

enum class EventSignal : int {
  NONE = 0,
  QUIT,
};

// TODO: Do not keep these variables globally allocated...
// {
std::queue<EventSignal> _event_queue;
std::mutex _event_queue_mutex;
std::thread _hs_thread;
std::mutex _start_gui_mutex;
bool _start_gui = false;

// TODO: Need cmake build to use clang to build otherwise these
// thread annotations don't matter...
std::mutex _renderer_ref_mu;
elmlike::Renderer *_renderer_ref ABSL_GUARDED_BY(_renderer_ref_mu);
// }

void signal_hs_shutdown() {
  std::scoped_lock l(_event_queue_mutex);
  _event_queue.push(EventSignal::QUIT);
}

EventSignal PollEventSignalInternal() {
  usleep(100'000);
  std::scoped_lock l(_event_queue_mutex);
  if (_event_queue.empty()) {
    return EventSignal::NONE;
  }
  const EventSignal sig = _event_queue.front();
  _event_queue.pop();
  return sig;
}

} // namespace

void StartGui() {
  std::scoped_lock l(_start_gui_mutex);
  _start_gui = true;
}

int PollEventSignal() { return static_cast<int>(PollEventSignalInternal()); }

void UiExec(std::function<void()> hs_entry) {
  printf("[UiExec] starting hs thread.\n");
  _hs_thread = std::thread(hs_entry);
  // TODO: Remove this sleep lol.
  while (true) {
    bool gui_started;
    {
      std::scoped_lock l(_start_gui_mutex);
      gui_started = _start_gui;
    }
    if (gui_started) {
      break;
    } else {
      usleep(100'000); // 100ms
    }
  }
  printf("[UiExec] received signal to start ui thread.\n");

  std::unique_ptr<elmlike::Renderer> renderer = elmlike::Renderer::Create();
  if (!renderer) {
    fprintf(stderr, "Failed to initialize renderer.\n");
    signal_hs_shutdown();
    _hs_thread.join();
    return;
  }
  {
    std::scoped_lock l(_renderer_ref_mu);
    _renderer_ref = renderer.get();
  }
  renderer->StartRenderLoop();
  signal_hs_shutdown();
  _hs_thread.join();
  printf("[UiExec] stopping hs thread.\n");
}

int GetNextDebugId() {
  static absl::Mutex id_mtx;
  absl::MutexLock l(&id_mtx);
  static int id = 0;
  return ++id;
}

void *MakeTextNode(const char *content, uint32_t size) {
  const int id = GetNextDebugId();
  printf("MakeTextNode Called: id=%d (%s)\n", id, content);
  // TODO: Refactor this node system to not do allocations.. these allocations
  // will happen every re-render.
  auto text = std::make_unique<TextNode>();
  text->size = size;
  text->content = content;

  auto new_node = std::make_unique<UiNode>();
  new_node->debug_id = std::to_string(id);
  new_node->priv = reinterpret_cast<void *>(text.release());
  return reinterpret_cast<void *>(new_node.release());
}

void ConnectNodesAtSameLevel(void *left_opaq, void *right_opaq) {
  UiNode *left = reinterpret_cast<UiNode *>(left_opaq);
  UiNode *right = reinterpret_cast<UiNode *>(right_opaq);
  assert(left);
  if (right != nullptr) {
    left->next = right;
    right->prev = left;
    printf("Connected %p (%s) to %p (%s)\n", left, left->debug_id.c_str(),
           right, right->debug_id.c_str());
  } else {
    left->next = nullptr;
    printf("Connect %p (%s) to %p\n", left, left->debug_id.c_str(), nullptr);
  }
}

void DrawNodes(void *head_opaq) {
  assert(head_opaq);

  { // Test draw -- todo actually queue the draw calls for each node
    std::scoped_lock l(_renderer_ref_mu);
    if (_renderer_ref != nullptr) {
      _renderer_ref->StartDrawPhase();

      UiNode *node = reinterpret_cast<UiNode *>(head_opaq);
      printf("head node=%p (%s)\n", node, node->debug_id.c_str());
      uint32_t nb_nodes_drawn = 0;
      while (node) {
        _renderer_ref->DrawNode(*node);
        node = node->next;
        ++nb_nodes_drawn;
      }
      _renderer_ref->EndDrawPhase();
      printf("Drew %u nodes\n", nb_nodes_drawn);
    }
  }
}
