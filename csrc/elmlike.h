#ifndef _ELMLIKE_H_
#define _ELMLIKE_H_

#include <cstdint>
#include <functional>

#if defined(__linux__) || defined(__APPLE__)
#define EXPORT __attribute__((visibility("default")))
#else
#define EXPORT __declspec(dllexport)
#endif

#ifdef __cplusplus
extern "C" {
#endif

EXPORT void StartGui();
EXPORT int PollEventSignal();
EXPORT void DrawText(const char *text);

void UiExec(std::function<void()> hs_entry);

// Creates a text node.
// Returns an opaque ptr to the newly created node.
EXPORT void *MakeTextNode(const char *content, uint32_t size);
// Connects the left and right node to each other, making left node
// ordered to the left or right. Left's current right becomes right's right.
EXPORT void ConnectNodesAtSameLevel(void *left_opaq, void *right_opaq);
EXPORT void DrawNodes(void *head_opaq);
// Find all nodes that are the left-most and push all nodes to the right so they
// are properly offset.
EXPORT void PushNodesToRight(void *head_opaq);

#ifdef __cplusplus
}
#endif

#endif // _ELMLIKE_H_
