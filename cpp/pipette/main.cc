#include <iostream>

#include "absl/memory/memory.h"

#include "pipette.h"

int main(int argc, char* argv[]) {
  std::cout << "Test App" << std::endl;
  int data[] = {1, 3, 5, 7, 9};
  auto src =
      absl::make_unique<ArraySource<int>>(data, sizeof(data)/sizeof(data[0]));
  int n;
  while (src->fetch(&n)) {
    std::cout << n << std::endl;
  }
  return 0;
}
