#include <functional>
#include <iostream>

#include "absl/memory/memory.h"

#include "pipette.h"

template<typename T>
void print(T t) {
  std::cout << t << std::endl;
}

template<typename T>
void run(Source<T>* src, std::function<void(T)> f) {
  T t;
  while (src->fetch(&t)) {
    f(t);
  }
}

int main(int argc, char* argv[]) {
  std::cout << "Test App" << std::endl;
  int data[] = {1, 3, 5, 7, 9};
  auto src =
      absl::make_unique<ArraySource<int>>(data, sizeof(data)/sizeof(data[0]));
  run<int>(src.get(), print<int>);
  return 0;
}
