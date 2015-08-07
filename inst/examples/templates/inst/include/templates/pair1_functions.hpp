#ifndef _TEMPLATES_PAIR1_FUNCTIONS_HPP_
#define _TEMPLATES_PAIR1_FUNCTIONS_HPP_

#include <templates/pair1.hpp>

namespace examples {

// Free functions that will use this:

// Taking a pair as an argument
template <typename T>
T sum(const pair1<T>& obj) {
  return obj.first + obj.second;
}

// Return a pair as return value
template <typename T>
pair1<T> make_pair1(const T& a, const T& b) {
  return pair1<T>(a, b);
}

// Take and return
template <typename T>
pair1<T> combine(const pair1<T>& a, const pair1<T>& b) {
  return pair1<T>(a.first + b.first, a.second + b.second);
}

}

#endif
