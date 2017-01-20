import collections
import inspect
import types
import six

PY3, PY2 = six.PY3, six.PY2


def make_cell(value):
    fn = (lambda x: lambda: x)(value)
    if PY3:
        return fn.__closure__[0]
    else:
        return fn.func_closure[0]


class Function(object):
    __slot__ = [
        'func_code',
        'func_name',
        'func_default',
        'func_globals',
        'func_locals',
        'func_dict',
        'func_closure',
        '__name__',
        '__dict__',
        '__doc__',
        '_vm',
        '_func',
    ]

    def __init__(self, name, code, globs, defaults, closure, vm):
        self._vm = vm
        self.func_code = code
        self.func_name = self.__name__ = name or code.co_name
        self.func_defaults = tuple(defaults)
        self.func_globals = globs
        self.func_locals = self._vm.frame.f_locals
        self.__dict__ = {}
        self.func_closure = closure
        self.__doc__ = code.co_consts[0] if code.co_consts else None
