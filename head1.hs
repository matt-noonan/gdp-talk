#include <iostream>
#include <vector>

int main() {
  std::vector<int> x{1};
  x.pop_back();
  std::cout << "Size is " << x.size()
           << ", head is " << x[0] << std::endl;
  return 0;
}
