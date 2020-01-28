from typing import ChainMap


class Value:
    def eval(self, ctx):
        return self


class Num(Value):
    def __init__(self, val):
        self.val = val

    def __add__(self, other):
        return Num(self.val + other.val)

    def __sub__(self, other):
        return Num(self.val - other.val)

    def __mul__(self, other):
        return Num(self.val * other.val)

    def __div__(self, other):
        return Num(self.val / other.val)


class Name(Value):
    def __init__(self, name):
        self.name = name

    def eval(self, ctx):
        return ctx[self.name]


class Fun(Value):
    def call(self, args): ...

    def __call__(self, *args):
        return self.call(args)


class PyFun(Fun):
    def __init__(self, fun):
        self.fun = fun

    def call(self, args):
        return self.fun(*args)
