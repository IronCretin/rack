from typing import ChainMap, Sequence, List, Iterator, Iterable, Callable,\
    Dict
from abc import ABC, abstractmethod
import re
import sys

Context = ChainMap[str, 'Value']


class Value(ABC):
    """
    Superclass for all lisp objects and expressions.
    """
    def run(self, ctx: Context) -> 'Value':
        """
        Evaluate the object in a context of variables, behavior depends on type
        """
        return self


class VList(Value):
    """
    List/S-Expression. When evaluated, if the first element is a literal
    name corresponding to a special form, exaluates according to form's rules,
    otherwise evaluates head and passes arguments to it as a function.
    """
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
        head = self.elems[0]
        if isinstance(head, Name) and head.name == 'quote':
            return self.elems[1]
        else:
            head = head.run(ctx)
            args = [e.run(ctx) for e in self.elems[1:]]
            if isinstance(head, Fun):
                return head.call(args)
            else:
                raise ValueError


class Num(Value):
    """
    A number, is either an int or a complex number, which are interoperable.
    """
    val: complex

    def __init__(self, val: complex) -> None:
        self.val = val

    def __add__(self, other: 'Num') -> 'Num':
        return Num(self.val + other.val)

    def __sub__(self, other: 'Num') -> 'Num':
        return Num(self.val - other.val)

    def __neg__(self) -> 'Num':
        return Num(-self.val)

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
    """
    A name, when evaluated returns the result of looking up the name in the
    variable context. Attempting to evaluate a reserved name will fail.
    """
    name: str

    def __init__(self, name: str) -> None:
        self.name = name

    def run(self, ctx: Context) -> Value:
        if self.name not in ('quote', 'lambda'):
            return ctx[self.name]
        else:
            raise ValueError

    def __str__(self):
        return self.name


class Fun(Value):
    """
    A function or other callable.
    """
    @abstractmethod
    def call(self, args: Sequence[Value]) -> Value: ...

    def __call__(self, *args):
        return self.call(args)


class PyFun(Fun):
    """
    Wrapper for a native python function, takes some list of Values and
    returns a Value
    """
    def __init__(self, name: str, fun: Callable[..., Value]):
        self.name = name
        self.fun = fun

    def call(self, args) -> Value:
        return self.fun(*args)

    def __str__(self):
        return f"#<function:{self.name}>"


def parse(inp: str) -> Iterator[Value]:
    """
    Parses an input string into expressions.
    """
    # This function tust tokenizes and passes the stream of tokens to the real
    # parser
    toks = iter(re.findall(r'\(|\)|\'|[^()\'\s]+', inp))
    for t in toks:
        yield _parse(t, toks)


def _parse(head: str, toks: Iterator[str]) -> Value:
    """
    Perses a stream of tokens into a syntax tree
    """
    if head == '(':
        lis: List[Value] = []
        for t in toks:
            if t == ')':
                return VList(lis)
            else:
                lis.append(_parse(t, toks))
        else:
            raise ValueError
    elif head == '\'':
        return VList([Name('quote'), _parse(next(toks), toks)])
    elif head[0] in '0123456789':
        if re.fullmatch(r'[+-]?(?:0[xbo])?\d+', head):
            return Num(int(head))
        else:
            return Num(complex(head))
    else:
        return Name(head)


def product(*nums: Iterable[Num]) -> Num:
    p = 1
    for n in nums:
        p *= n.val
    return Num(p)


stdlib: Dict[str, Value] = {
    # arithmetic and numbers
    '+': PyFun('+', lambda *args: sum(args)),
    '-': PyFun('-', lambda a, b=None: -a if b is None else a - b),
    '*': PyFun('*', product),
    '/': PyFun('-', lambda a, b: a / b),
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


run(parse(' '.join(sys.argv[1:])), None, True)
