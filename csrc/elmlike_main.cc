#include <cstdio>
#include "ElmLike_stub.h"

int main (int argc, char **argv) {
  printf("Elmlike Main.\n");
  printf("Running exampleProgram.\n");

  hs_init(&argc, &argv);
  exampleProgram();
  hs_exit();

  return 0;
}
