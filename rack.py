from typing import ChainMap, Sequence, List, Iterator, Iterable, Callable, Dict
from abc import ABC, abstractmethod
import re
import sys

Context = ChainMap[str, 'Value']


class Value(ABC):
    def run(self, ctx: Context) -> 'Value':
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

    def run(self, ctx: Context) -> Value:
        head = self.elems[0].run(ctx)
        args = [e.run(ctx) for e in self.elems[1:]]
        if isinstance(head, Fun):
            return head.call(args)
        else:
            raise ValueError


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

    def __str__(self):
        if self.val.imag == 0:
            return str(self.val.real)
        else:
            return str(self.val)


class Name(Value):
    name: str

    def __init__(self, name: str) -> None:
        self.name = name

    def run(self, ctx: Context) -> Value:
        return ctx[self.name]

    def __str__(self):
        return self.name


class Fun(Value):
    @abstractmethod
    def call(self, args: Sequence[Value]) -> Value: ...

    def __call__(self, *args):
        return self.call(args)


class PyFun(Fun):
    def __init__(self, name: str, fun: Callable[..., Value]):
        self.name = name
        self.fun = fun

    def call(self, args) -> Value:
        return self.fun(*args)

    def __str__(self):
        return f"#<function:{self.name}>"


def parse(inp: str) -> Iterator[Value]:
    toks = iter(re.findall(r'\(|\)|[^()\s]+', inp))
    for t in toks:
        yield _parse(t, toks)


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
        if re.fullmatch(r'[+-]?(?:0[xbo])?\d+', head):
            return Num(int(head))
        else:
            return Num(complex(head))
    else:
        return Name(head)


stdlib: Dict[str, Value] = {
    '+': PyFun('+', lambda *args: Num(sum(a.val for a in args)))
}


def run(exps: Iterable[Value],
        ctx: Context = None, show: bool = False) -> Value:
    res = VList([])
    if ctx is None:
        ctx = ChainMap({}, stdlib)
    for e in exps:
        res = e.run(ctx)
        if show:
            print(res)
    return res


run(parse(sys.argv[1]), None, True)
