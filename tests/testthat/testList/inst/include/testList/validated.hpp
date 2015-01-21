#ifndef _TESTEXAMPLES_VALIDATED_HPP_
#define _TESTEXAMPLES_VALIDATED_HPP_

#include <vector>

namespace examples {

// Here is the thing that we want to export.
struct validated {
public:
  int n_elements;
  std::vector<double> list;
  validated() : n_elements(0) {}
  void validate() const;
};

}

#endif
