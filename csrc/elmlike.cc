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

namespace {

enum class EventSignal : int {
  NONE = 0,
  QUIT,
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

void StartGui() { _start_gui = true; }

int PollEventSignal() { return static_cast<int>(PollEventSignalInternal()); }

void UiExec(std::function<void()> hs_entry) {
  printf("[UiExec] starting hs thread.\n");
  _hs_thread = std::thread(hs_entry);
  // TODO: Remove this sleep lol.
  while (!_start_gui) {
    usleep(100'000); // 100ms
  }
  printf("[UiExec] received signal to start ui thread.\n");

  std::unique_ptr<elmlike::Renderer> renderer = elmlike::Renderer::Create();
  if (!renderer) {
    fprintf(stderr, "Failed to initialize renderer.\n");
    signal_hs_shutdown();
    _hs_thread.join();
    return;
  }
  renderer->StartRenderLoop();
  signal_hs_shutdown();
  _hs_thread.join();
  printf("[UiExec] stopping hs thread.\n");
}

struct TextNode {
  std::string content;
  uint32_t size;
};

struct UiNode {
  UiNode *prev, *next;
  void *priv;
};

void* MakeTextNode(const char *content, uint32_t size) {
  // TODO: Refactor this node system to not do allocations.. these allocations
  // will happen every re-render.
  auto text = std::make_unique<TextNode>();
  text->size = size;
  text->content = content;

  auto new_node = std::make_unique<UiNode>();
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
    printf("Connected %p to %p\n", left, right);
  } else {
    left->next = nullptr;
  }
}

void DrawNodes(void *head_opaq) {
  assert(head_opaq);

  UiNode *node = reinterpret_cast<UiNode *>(head_opaq);
  while (node) {
    node = node->next;
  }
}
