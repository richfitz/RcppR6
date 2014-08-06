#ifndef _TESTTEMPLATES_PAIR1_HPP_
#define _TESTTEMPLATES_PAIR1_HPP_

#include <utility>

namespace pair {

// Simple homgeneous pair.
template <typename T>
class pair1 {
public:
  typedef T data_type;
  pair1(const T& first_, const T& second_)
    : first(first_), second(second_) {}
  T first;
  T second;
};

// Free functions that will use this:

// Taking a pair as an argument
template <typename T>
T sum(const pair1<T>& obj) {
  return obj.first + obj.second;
}

// Return a pair as return value
template <typename T>
pair1<T> make_pair(const T& a, const T& b) {
  return pair1<T>(a, b);
}

// Take and return
template <typename T>
pair1<T> combine(const pair1<T>& a, const pair1<T>& b) {
  return pair1<T>(a.first + b.first, a.second + b.second);
}

}

#endif
