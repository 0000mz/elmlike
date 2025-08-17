#ifndef _ELMLIKE_RENDERER_H_
#define _ELMLIKE_RENDERER_H_

#include <memory>

namespace elmlike {

struct RendererInternal;
class Renderer {
public:
  ~Renderer();

  static std::unique_ptr<Renderer> Create();

  void StartRenderLoop();

private:
  Renderer() = default;
  bool Init();

  std::unique_ptr<RendererInternal> renderer_;
};
}

#endif  // _ELMLIKE_RENDERER_H_
