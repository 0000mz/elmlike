#include <gtest/gtest.h>
#include <unistd.h>

#include "elmlike.h"

TEST(GuiTest, RunGui) {
  start_gui();
  UiExec([]() {});
}
