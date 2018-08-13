#include <cstdint>
#include <iostream>

uint_fast64_t sq_chain(uint_fast64_t n) {
  uint_fast64_t sum = 0;
  while (n > 0) {
    uint_fast8_t digit = n % 10;
    sum += (digit * digit);
    n /= 10;
  }
  return sum;
}

bool is89(uint_fast64_t n) {
  while(true) {
    if (n == 1) return false;
    if (n == 89) return true;
    n = sq_chain(n);
  }
}

int main(int argc, char** argv) {
  uint_fast64_t c = 0;
  for (uint_fast64_t i = 1; i <= 10000000; ++i) {
    if (is89(i)) ++c;
  }
  std::cout << c << std::endl;
  return 0;
}
