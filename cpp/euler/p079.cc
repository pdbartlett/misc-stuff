#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

bool is_consistent(const std::string& response, const std::string& guess) {
  size_t pos = 0;
  for (int i = 0; i < response.size(); ++i) {
    pos = guess.find(response[i], pos);
    if (pos == std::string::npos) return false;
  }
  return true;
}

int main(int argc, char** argv) {
  std::ifstream infile("/Users/pdbartlett/src/cpp/euler/p079_keylog.txt");
  std::vector<std::string> responses;
  std::string response;
  while (std::getline(infile, response)) {
    responses.push_back(response);
  }
  for (int i = 100; ; ++i) {
    std::string guess(std::to_string(i));
    if (std::all_of(responses.cbegin(), responses.cend(),
                    [&guess](std::string response){ return is_consistent(response, guess); })) {
      std::cout << guess << std::endl;
      break;
    }
  }
}
