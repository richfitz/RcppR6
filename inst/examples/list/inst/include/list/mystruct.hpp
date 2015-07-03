#ifndef _TESTEXAMPLES_MYSTRUCT_HPP_
#define _TESTEXAMPLES_MYSTRUCT_HPP_

#include <string>

namespace examples {

// Here is the thing that we want to export.
struct mystruct {
public:
  bool a_bool;
  int  an_int;
  double a_real_number;
  std::string a_string;
  mystruct()
    : a_bool(true),
      an_int(3),
      a_real_number(3.141),
      a_string("hello world") {}
};

}

#endif
