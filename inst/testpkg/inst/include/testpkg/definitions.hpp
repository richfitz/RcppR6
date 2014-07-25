#ifndef _TESTPKG_DEFINITIONS_HPP_
#define _TESTPKG_DEFINITIONS_HPP_

namespace testpkg {

// A simple class to wrap
class counter {
public:
  counter() : a(0) {}
  // const method
  int value() {return a;}
  // non-const method
  void increment() {++a;}
  // non-const method with arguments
  int set_value(int value) {
    // std::swap(value, a);
    // return value;
    int ret = a;
    a = value;
    return ret;
  }
private:
  int a;
};

}
