#include "ElmLike_stub.h"
#include "elmlike.h"


#include <functional>
#include <cstdio>

int main (int argc, char **argv) {
  printf("Elmlike Main.\n");
  printf("Running exampleProgram.\n");

  // TODO: Need to wait for the UI execution to be ready
  // before executing the hs_thread.
  std::function<void()> hs_entry = [&argc, &argv]() {
    hs_init(&argc, &argv);
    exampleProgram();
    hs_exit();
  };
  UiExec(hs_entry);
  return 0;
}
