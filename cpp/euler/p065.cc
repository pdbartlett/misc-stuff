#include <iostream>
#include <string>

#include "bigint.h"

using ::Dodecahedron::Bigint;

class fraction {
 private:
  Bigint n_;
  Bigint d_;
 public:
  fraction(Bigint integer) : n_(integer), d_(1) {}
  fraction(Bigint n, Bigint d) : n_(n), d_(d) {}

  void print(const std::string& s) {
    std::cout << s << ": " << n_ << " / " << d_ << std::endl;
  }

  void add(Bigint integer) {
    n_ += (integer * d_);
  }

  void recip() {
    Bigint temp = d_;
    d_ = n_;
    n_ = temp;
  }

  Bigint n() const { return n_; }
  Bigint d() const { return d_; }
};

template<typename F> fraction icf(uint_fast64_t base, F gen, int count) {
  fraction f = (count == 0) ? fraction(0, 1) : fraction(1, gen(count));
  for (int i = count - 1; i > 0; --i) {
    f.add(gen(i));
    f.recip();
  }
  f.add(base);
  return f;
}

int main(int argc, char** argv) {
  fraction ans(42); ans.print("ans");
  fraction pi(22, 7); pi.print("pi");

  for (int i = 0; i < 7; ++i) {
    fraction sqrt2 = icf(1, [](Bigint) -> Bigint { return 2; }, i);
    sqrt2.print("sqrt2");
  }

  auto efn = [](uint_fast64_t n) -> Bigint { return (n % 3 == 2) ? 2 * (n + 1) / 3 : 1; };
  for (int i = 0; i < 11; ++i) {
    fraction e = icf(2, efn, i);
    e.print("e");
  }

  fraction important = icf(2, efn, 99);
  important.print("imp");

  int sum = 0;
  Bigint num = important.n();
  for (int i = 0; i < num.digits(); ++i) {
    sum += num[i];
  }
  std::cout << "Final answer: " << sum << std::endl;

  return 0;
}
