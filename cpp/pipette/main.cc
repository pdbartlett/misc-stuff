#include <functional>
#include <iostream>

#include "absl/memory/memory.h"

#include "pipette.h"

template<typename T>
void print(T t) {
  std::cout << t << std::endl;
}

int main(int argc, char* argv[]) {
  int data[] = {1, 3, 5, 7, 9};
  auto src =
      absl::make_unique<ArraySource<int>>(data, sizeof(data)/sizeof(data[0]));
  LocalPipeline<int> p(std::move(src));
  p.source()->set_sink(print<int>);
  p.run();
  return 0;
}
