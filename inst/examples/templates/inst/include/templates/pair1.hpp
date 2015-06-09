#ifndef _TEMPLATES_PAIR1_HPP_
#define _TEMPLATES_PAIR1_HPP_

namespace examples {

template <typename T>
class pair1 {
public:
  typedef T data_type;
  pair1(const T& first_, const T& second_)
    : first(first_), second(second_) {}
  T first;
  T second;
};

}

#endif
