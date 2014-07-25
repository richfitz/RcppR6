## Relatively straightforward dependencies.  The 'yaml' module comes
## from pyyaml, which can be installed with 
##    sudo pip install pyyaml
## string is a builtin module.
import yaml
from string import Template

def read_yml(filename):
    """Read contents of a yml file, closing it afterwards"""
    f = open(filename)
    try:
        dat = yaml.safe_load(f)
    finally:
        f.close()
    return dat

def tp(template, d):
    """Simple wrapper to help with templating"""
    return Template(template).substitute(d)

def indent(str, n):
    """Simple-minded text indenting.  In python 3, textwrap.indent
    would do this for us"""
    indent = ' ' * n
    return '\n'.join([indent + s for s in str.split('\n')])

def dict_merge(a, b):
    """Merge dictionaries 'a' and 'b', taking elements from 'b' over
    'a' if there are duplicates"""
    c = a.copy()
    c.update(b)
    return c

def roxygen_prepare(str):
    if str:
        return "\n".join(["##' " + s for s in str.strip().split('\n')])
    else:
        return ""

class Class:
    def __init__(self, name, defn):
        # There are no required keys here?  'constructor'?
        self.classname = name
        self.namespace = defn.get("namespace", None)
        self.package = defn.get("package", "global")
        self.methods = [Method(method_name, method_defn or {}, self) 
                        for method_name, method_defn in 
                        defn.get("methods", {}).iteritems()]
        self.constructor = Constructor(defn['constructor'] or {}, self)
    def name(self):
        return self.classname
    def name_full(self):
        return self.namespace + '::' + self.classname
    def generator(self):
        return '.R6_%s' % self.name()
    # These are used in overall marshalling, and are common to all
    # generated code:
    def ptr_type(self):
        return 'Rcpp::XPtr<%s>' % self.name_full()
    def ptr_name(self):
        return 'ptr_'
    def input_type(self):
        return 'Rcpp::RObject' # or SEXP?
    def input_name(self):
        return 'obj_'
    def r_self_name(self):
        return 'self'
    def template_info(self):
        return {'classname_full': self.name_full(),
                'classname':      self.name(),
                'generator':      self.generator(),
                'ptr_type':       self.ptr_type(),
                'ptr_name':       self.ptr_name(),
                'input_type':     self.input_type(),
                'input_name':     self.input_name(),
                'package':        self.package}
    def rcpp_wrap_prototype(self):
        template = "template <> SEXP wrap(const ${classname_full}&);"
        return tp(template, self.template_info())
    def rcpp_wrap_definition(self):
        template = \
"""template <> inline SEXP wrap(const ${classname_full}& x) {
  using generation::support::ptr_to_R6;
  return ptr_to_R6(x, "${classname}", "${generator}", "${package}");
}"""
        return tp(template, self.template_info())
    def rcpp_as_prototype(self):
        template = "template <> ${classname_full} as(SEXP);"
        return tp(template, self.template_info())
    def rcpp_as_definition(self):
        template = \
"""template <> inline ${classname_full} as(SEXP x) {
  using generation::support::ptr_from_R6;
  return *ptr_from_R6<${classname_full}>(x, "${classname}");
}"""
        return tp(template, self.template_info())
    def rcpp_pre(self):
        return '\n'.join([self.rcpp_wrap_prototype(),
                          self.rcpp_as_prototype()])
    def rcpp_post(self):
        return '\n'.join([self.rcpp_wrap_definition(),
                          self.rcpp_as_definition()])
    def r6_generator(self):
        template = \
"""${generator} <-
  R6::R6Class("${classname}",
              public=list(
                initialize = function(ptr) 
                  {ptr <<- ptr},
${methods}
                ),
              private=list(ptr=NULL))"""
        d = self.template_info()
        d['methods'] = indent(self.methods_r(), 16)
        return tp(template, d)
    def methods_cpp(self):
        return '\n'.join([m.cpp() for m in self.methods])
    def methods_r(self):
        return ',\n'.join([m.r() for m in self.methods])
    def constructor_cpp(self):
        return self.constructor.cpp()
    def constructor_r(self):
        return self.constructor.r()
    def cpp(self):
        return '\n'.join([self.constructor_cpp(), self.methods_cpp()])
    def r(self):
        return '\n'.join([self.constructor_r(), self.r6_generator()])

## Need a bit more for active binding functions: might be best to put
## them into their own class, but I might inherit from this...
class Method:
    def __init__(self, name, defn, klass):
        # Required keys: 'method', 'return'
        self.klass       = klass
        self.method      = name
        self.method_cpp  = defn.get('method_cpp', self.method)
        self.member      = defn.get('member', True)
        self.args        = Args(defn.get('args', []), self)
        self.return_type = defn['return']
    def return_statement(self):
        return '' if self.return_type == 'void' else 'return '
    def template_info(self):
        """Internal template information"""
        info_class = self.klass.template_info()
        info_method = {'method':           self.method,
                       'method_cpp':       self.method_cpp,
                       'args_cpp_defn':    self.args.cpp_defn(),
                       'args_cpp_use':     self.args.cpp_use(),
                       'args_r_defn':      self.args.r_defn(),
                       'args_r_use':       self.args.r_use(),
                       'return_type':      self.return_type,
                       'return_statement': self.return_statement()}
        return dict_merge(info_class, info_method)
    def cpp(self):
        """Method definition for C++"""
        if self.member:
            template_call = '${ptr_name}->${method_cpp}(${args_cpp_use})'
        else:
            template_call = '${method_cpp}(${args_cpp_use})'
        template = \
"""// [[Rcpp::export]]
${return_type} ${classname}__${method}(${args_cpp_defn}) {
  using generation::support::ptr_from_R6;
  ${ptr_type}
    ${ptr_name}(ptr_from_R6<${classname_full}>(${input_name}, "${classname}"));
  ${return_statement}%s;
}""" % template_call
        return tp(template, self.template_info())
    def r(self):
        template = \
"""${method} = function(${args_r_defn})
  {${classname}__${method}(${args_r_use})}"""
        return tp(template, self.template_info())

class Constructor:
    def __init__(self, defn, klass):
        self.klass = klass
        self.args = Args(defn.get('args', []), self)
        self.roxygen = roxygen_prepare(defn.get('roxygen', None))
    def cpp(self):
        """Constructor definition for C++"""
        template = \
"""// [[Rcpp::export]]
${classname_full} ${classname}__ctor(${args_cpp_defn}) {
  return ${classname_full}(${args_cpp_use});
}"""
        return tp(template, self.template_info())
    def r(self):
        template = \
"""${roxygen}
${classname} <- function(${args_r_defn}) {
  ${classname}__ctor(${args_r_use})
}"""
        return tp(template, self.template_info())
    def template_info(self):
        info_class = self.klass.template_info()
        info_constructor = {'args_cpp_defn':    self.args.cpp_defn(),
                            'args_cpp_use':     self.args.cpp_use(),
                            'args_r_defn':      self.args.r_defn(),
                            'args_r_use':       self.args.r_use(),
                            'roxygen':          self.roxygen}
        return dict_merge(info_class, info_constructor)
    def roxygen_r(self):
        if self.roxygen:
            str.split(self.roxygen)

class Args:
    def __init__(self, args, method):
        if type(args) is not list:
            raise Exception("args must be a list")
        if not all([type(i) is dict and len(i) == 1 for i in args]):
            raise Exception("All args must be single-element dictionaries")
        self.method = method
        self.klass  = method.klass
        self.args_name = []
        self.args_type = []
        self.constructor = not hasattr(self.method, "member")
        for i in args:
            n, t = i.items()[0]
            self.args_name.append(str(n))
            self.args_type.append(str(t))
    def cpp_defn(self):
        types = self.args_type
        names = self.args_name
        if not self.constructor:
            types = [self.klass.input_type()] + types
            names = [self.klass.input_name()] + names
        return ', '.join(' '.join(i) for i in zip(types, names))
    def cpp_use(self):
        if self.constructor or self.method.member:
            return ', '.join(self.args_name)
        else:
            return ', '.join(['*' + self.klass.ptr_name()] + self.args_name)
    def r_defn(self):
        return ', '.join(self.args_name)
    def r_use(self):
        names = self.args_name
        if not self.constructor:
            names = [self.klass.r_self_name()] + names
        return ', '.join(names)

def load(filename):
    classes = read_yml(filename)
    return [Class(*args) for args in classes.items()]

def r(classes):
    return "\n\n".join(obj.r() for obj in classes)
def cpp(classes):
    return "\n\n".join(obj.cpp() for obj in classes)
def rcpp_pre(classes):
    template = \
"""
#include <RcppCommon.h>
namespace Rcpp {
${body}
}
"""
    body = "\n\n".join(obj.rcpp_pre() for obj in classes)
    return tp(template, {'body': body})
def rcpp_post(classes):
    template = \
"""
#include <Rcpp.h>
namespace Rcpp {
${body}
}
"""
    body = "\n\n".join(obj.rcpp_post() for obj in classes)
    return tp(template, {'body': body})
