#ifndef _ELMLIKE_RENDERER_H_
#define _ELMLIKE_RENDERER_H_

#include <memory>
#include <string>

namespace elmlike {

struct TextNode {
  std::string content;
  uint32_t size;
};

struct UiNode {
  UiNode *prev, *next;
  std::string debug_id;
  unsigned int width, height, x, y;
  void *priv;
};

struct RendererInternal;
class Renderer {
public:
  ~Renderer();

  static std::unique_ptr<Renderer> Create();

  void StartRenderLoop();

  // `{Start,End}DrawPhase` should be used for batching multiple draw
  // commands into a single draw phase.
  // When `EndDrawPhase` is called, all draw commands will be committed.
  void StartDrawPhase();
  void EndDrawPhase();
  void DrawNode(const UiNode &node);

private:
  Renderer() = default;
  bool Init();

  std::unique_ptr<RendererInternal> renderer_;
};
} // namespace elmlike

#endif // _ELMLIKE_RENDERER_H_
