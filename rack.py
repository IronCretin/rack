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
        if isinstance(head, Name):
            if head.name == 'quote':
                return self.elems[1]
            elif head.name == 'if':
                pred = self.elems[1].run(ctx)
                if isinstance(pred, Bool) and pred.val == False:
                    return self.elems[3].run(ctx)
                else:
                    return self.elems[2].run(ctx)
            elif head.name == 'lambda':
                return Lambda([a.name for a in self[1]], self[2:], ctx)
            elif head.name == 'define':
                if isinstance(self[1], VList):
                    ctx[self[1][0].name] = Lambda(
                        [a.name for a in self[1][1:]],
                        self[2:], ctx, self[1][0].name)
                else:
                    ctx[self[1].name] = self[2].run(ctx)
                return VList([])
        head = head.run(ctx)
        args = [e.run(ctx) for e in self.elems[1:]]
        if isinstance(head, Fun):
            return head.call(args)
        raise ValueError("Invalid expression: {head} is not a function.")


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


class Bool(Value):
    val: bool

    def __init__(self, val: bool) -> None:
        self.val = val

    def __and__(self, other: 'Bool') -> 'Bool':
        return Bool(self.val and other.val)

    def __or__(self, other: 'Bool') -> 'Bool':
        return Bool(self.val or other.val)

    def __invert__(self) -> 'Bool':
        return Bool(not self.val)

    def __bool__(self) -> bool:
        return self.val

    def __str__(self):
        if self.val:
            return '#t'
        else:
            return '#f'


class Name(Value):
    """
    A name, when evaluated returns the result of looking up the name in the
    variable context. Attempting to evaluate a reserved name will fail.
    """
    name: str

    def __init__(self, name: str) -> None:
        self.name = name

    def run(self, ctx: Context) -> Value:
        if self.name not in ('quote', 'if', 'lambda', 'define'):
            return ctx[self.name]
        else:
            raise ValueError(f"Reserved name: {self.name}")

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
        return f"#<native function:{self.name}>"


class Lambda(Fun):
    def __init__(self, args: List[str], stmts: List[Value], closure: Context,
            name: str = 'lambda'):
        self.args = args
        self.statements = stmts
        self.closure = closure
        self.name = name

    def call(self, args):
        ctx = self.closure.new_child(dict(zip(self.args, args)))
        return run(self.statements, ctx)

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
            raise ValueError("List not terminated")
    elif head == '\'':
        return VList([Name('quote'), _parse(next(toks), toks)])
    elif re.match(r'[+-]?\d', head):
        if re.fullmatch(r'[+-]?(?:0[xbo])?\d+', head):
            return Num(int(head, base=0))
        else:
            return Num(complex(head))
    elif head[0] == '#':
        if head == '#t':
            return Bool(True)
        elif head == '#f':
            return Bool(False)
        else:
            raise ValueError(f"Invalid expression: {head}")
    else:
        return Name(head)


def product(*nums: Iterable[Num]) -> Num:
    p = 1
    for n in nums:
        p *= n.val
    return Num(p)


stdlib: Dict[str, Value] = {
    # arithmetic and numbers
    '+': PyFun('+', lambda *args: Num(sum(a.val for a in args))),
    '-': PyFun('-', lambda a, b=None: -a if b is None else a - b),
    '*': PyFun('*', product),
    '/': PyFun('-', lambda a, b: a / b),
    'num?': PyFun('num?', lambda n: Bool(isinstance(n, Num))),
    '<': PyFun('<', lambda a, b: Bool(a.val < b.val)),
    '>': PyFun('>', lambda a, b: Bool(a.val > b.val)),
    '<=': PyFun('<=', lambda a, b: Bool(a.val <= b.val)),
    '>=': PyFun('>=', lambda a, b: Bool(a.val >= b.val)),
    '=': PyFun('=', lambda a, b: Bool(a.val == b.val)),
    # booleans
    'and': PyFun('and', lambda *args: Bool(all(args))),
    'or': PyFun('or', lambda *args: Bool(any(args))),
    'not': PyFun('not', lambda b: ~b),
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
