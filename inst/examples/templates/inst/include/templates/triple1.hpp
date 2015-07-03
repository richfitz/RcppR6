#ifndef _TEMPLATES_TRIPLE1_HPP_
#define _TEMPLATES_TRIPLE1_HPP_

namespace examples {
// In contrast to the pair1 type, this is going to need to be
// default constructible to work with list classes.
template <typename T>
class triple1 {
public:
  typedef T data_type;
  T first;
  T second;
  T third;
};
}

#endif
