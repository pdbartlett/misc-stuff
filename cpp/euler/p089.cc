#include <algorithm>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <sstream>
#include <string>

int letter_value(char letter) {
  switch (letter) {
    case 'I': return 1;
    case 'V': return 5;
    case 'X': return 10;
    case 'L': return 50;
    case 'C': return 100;
    case 'D': return 500;
    case 'M': return 1000;
    default:  std::string msg("letter not recognised: ");
              throw std::invalid_argument(msg + letter);
  }
}

bool try_group(int* dec, int threshold, std::string group, std::string* roman) {
  if (*dec < threshold) return false;
  *dec -= threshold;
  *roman += group;
  return true;
}

int roman_to_dec(const std::string& roman) {
  int dec = 0;
  size_t cch = roman.length();
  for (int i = 0; i < cch; ) {
    int this_value = letter_value(roman[i]);
    int next_value = (++i < cch) ? letter_value(roman[i]) : 0;
    if (this_value >= next_value) {
      dec += this_value;
    } else {
      dec -= this_value;
      dec += next_value;
      ++i;
    }
  }
  return dec;
}

std::string dec_to_roman(int dec) {
  std::string roman;
  while (dec > 0) {
    if (try_group(&dec, 1000, "M", &roman)) {}
    else if (try_group(&dec, 900, "CM", &roman)) {}
    else if (try_group(&dec, 500, "D", &roman)) {}
    else if (try_group(&dec, 400, "CD", &roman)) {}
    else if (try_group(&dec, 100, "C", &roman)) {}
    else if (try_group(&dec, 90, "XC", &roman)) {}
    else if (try_group(&dec, 50, "L", &roman)) {}
    else if (try_group(&dec, 40, "XL", &roman)) {}
    else if (try_group(&dec, 10, "X", &roman)) {}
    else if (try_group(&dec, 9, "IX", &roman)) {}
    else if (try_group(&dec, 5, "V", &roman)) {}
    else if (try_group(&dec, 4, "IV", &roman)) {}
    else if (try_group(&dec, 1, "I", &roman)) {}
    else throw std::logic_error("Huh?");
  }
  return roman;
}

void test(std::string roman, int dec) {
  std::cout << roman << "->" << roman_to_dec(roman) << std::endl;
  std::cout << dec << "->" << dec_to_roman(dec) << std::endl;
}

int main(int argc, char** argv) {
  std::cout << "X->" << letter_value('X') << std::endl;
  try {
    letter_value('Y');
    std::cout << "SHOULDN'T GET HERE!!!" << std::endl;
  } catch (std::invalid_argument& e) {
    std::cout << "Y rightly throws exception '" << e.what() << "'" << std::endl;
  }
  test("III", 3);
  test("IV", 4);
  test("XV", 15);
  test("MCMXCIX", 1999);

  // Problem itself.
  std::ifstream infile("/Users/pdbartlett/src/cpp/euler/p089_roman.txt");
  int cch = 0;
  std::string roman;
  while (std::getline(infile, roman))
  {
    int dec = roman_to_dec(roman);
    std::string minimal(dec_to_roman(dec));
    std::cout << roman << ", " << dec << ", " << minimal << std::endl;
    cch += roman.size();
    cch -= minimal.size();
  }
  std::cout << "Characters saved = " << cch << std::endl;

  return 0;
}
