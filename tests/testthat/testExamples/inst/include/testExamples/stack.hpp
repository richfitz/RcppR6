#ifndef _TESTEXAMPLES_STACK_HPP_
#define _TESTEXAMPLES_STACK_HPP_

#include <stack>
#include <R.h> // NA_INTEGER

namespace examples {

// We're going to wrap std::stack.  There's pretty much no need to do
// this in this way (simply copying elements would be fine).  This is
// not yet templated on type, but that's coming.

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
// we can easily crash R, which is impolite.  I'm putting these in
// their own namespace, but that's entirely optional.
namespace stack_internals {
inline void pop(stack& x) {
  if (!x.empty()) {
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
}

// There are also non-member relational operators to map, such as the
// equality operator.  These have odd syntax, so it might be easiest
// to implement with a free function (Note that this *must* be
// declared inline unless the guts of it are moved into a .cpp file).
inline bool stack_eq(const stack& self,const stack& other) {
  // may be possible to do
  // self.operator==(other);
  // operator==(self, other);
  return self == other;
}

// Alternatively, this can be implemented without a wrapper: see the
// 'differs' entry.

// Actually wrapping these things out to implement native R operators
// is not supported yet.

}

#endif
