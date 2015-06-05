#ifndef _EXAMPLES_STACK_HPP_
#define _EXAMPLES_STACK_HPP_

#include <stack>
#include <RcppCommon.h> // NA_INTEGER

namespace examples {

//  I'm going to do this via a typedef just to make this easier.
//  However, that's not strictly needed.
typedef std::stack<int> stack;

// We'll expose methods:
//   * empty() (boolean)
//   * size() (size_t)
//   * top() (int)
//   * pop() (void)
//   * push() (void)

// Safer versions of top, pop are desirable though, because otherwise
// we can easily crash R, which is impolite.
inline void pop(stack& x) {
  if (x.empty()) {
    Rcpp::stop("empty stack");
  } else {
    x.pop();
  }
}
inline int top(const stack& x) {
  if (x.empty()) {
    return NA_INTEGER;
  } else {
    return x.top();
  }
}

// There are also non-member relational operators to map, such as the
// equality operator.  These have odd syntax, so it might be easiest
// to implement with a free function (Note that this *must* be
// declared inline unless the guts of it are moved into a .cpp file).
inline bool stack_eq(const stack& self, const stack& other) {
  return self == other;
}

// Alternatively, this can be implemented without a wrapper: see the
// 'differs' entry.

}

#endif
