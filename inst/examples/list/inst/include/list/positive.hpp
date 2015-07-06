#ifndef _TEMPLATES_POSITIVE_HPP_
#define _TEMPLATES_POSITIVE_HPP_

#include <RcppCommon.h>

namespace examples {
template <typename T>
class positive {
public:
  typedef T data_type;
  T value;
  void validate() const {
    if (value < 0) {
      Rcpp::stop("value must be positive");
    }
  }
};
}

#endif
