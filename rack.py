from typing import ChainMap, Sequence, List, Iterator
from abc import ABC, abstractmethod
import re

Context = ChainMap[str, 'Value']


class Value(ABC):
    def eval(self, ctx: Context) -> 'Value':
        return self


class VList(Value):
    elems: List[Value]

    def __init__(self, elems: List[Value]) -> None:
        self.elems = elems

    def __getitem__(self, i: int) -> Value:
        return self.elems[i]

    def __len__(self) -> int:
        return len(self.elems)
    
    def __str__(self):
        return f"({' '.join(map(str, self.elems))})"


class Num(Value):
    val: complex

    def __init__(self, val: complex) -> None:
        self.val = val

    def __add__(self, other: 'Num') -> 'Num':
        return Num(self.val + other.val)

    def __sub__(self, other: 'Num') -> 'Num':
        return Num(self.val - other.val)

    def __mul__(self, other: 'Num') -> 'Num':
        return Num(self.val * other.val)

    def __div__(self, other: 'Num') -> 'Num':
        return Num(self.val / other.val)


class Name(Value):
    name: str

    def __init__(self, name: str) -> None:
        self.name = name

    def eval(self, ctx: Context) -> Value:
        return ctx[self.name]


class Fun(Value):
    @abstractmethod
    def call(self, args: Sequence[Value]) -> Value: ...

    def __call__(self, *args):
        return self.call(args)


class PyFun(Fun):
    def __init__(self, fun):
        self.fun = fun

    def call(self, args) -> Value:
        return self.fun(*args)


def parse(inp: str) -> Iterator[Value]:
    toks = iter(re.findall(r'\(|\)|[^()\s]+', inp))
    for t in toks:
        yield _parse(next(toks), toks)


def _parse(head: str, toks: Iterator[str]) -> Value:
    if head == '(':
        lis: List[Value] = []
        for t in toks:
            if t == ')':
                return VList(lis)
            else:
                lis.append(_parse(t, toks))
        else:
            raise ValueError
    elif head[0] in '0123456789':
        if re.fullmatch(r'[+-]?(?:0[xbo])\d+', head):
            return Num(int(head))
        else:
            return Num(complex(head))
    else:
        return Name(head)

