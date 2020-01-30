# Incredibly cursed hack, basically a decorator that lets you use a class as a
# table of functions
from typing import Dict, Any


def mk_dict(cls: type) -> Dict[str, Any]:
    d = {}
    for k, v in cls.__dict__.items():
        if len(k) >= 2 and k[:2] == '__' and k[-2:] == '__':
            continue
        if isinstance(v, key):
            d[v.key] = v.val
        else:
            d[k] = v
    return d


class key:
    def __init__(self, key):
        self.key = key

    def __call__(self, val):
        self.val = val
        return self
