#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import math

def fermat_factor(n):
    """http://en.wikipedia.org/wiki/Fermat%27s_factorization_method
   
    >>> fermat_factor(5959)
    (59, 101)
    >>> fermat_factor(1342127)
    (1051, 1277)
    >>> fermat_factor(2)
    Traceback (most recent call last):
      ...
    ValueError: n must be odd
    >>> fermat_factor(125)
    (5, 25)
    >>> fermat_factor(9)
    (3, 3)
    >>> fermat_factor(7)
    (1, 7)
    >>> fermat_factor(3)
    (1, 3)
    >>> fermat_factor(11)
    (1, 11)
    >>> fermat_factor(0)
    Traceback (most recent call last):
      ...
    ValueError: n must be > 0
    """
    if n <= 0:
        raise ValueError('n must be > 0')
    q, r = divmod(int(list(str(n))[-1]), 2)
    if r == 0:
        raise ValueError('n must be odd')
    x = math.trunc(math.sqrt(n))
    if n == math.pow(x, 2):
        return x, x
    while True:
        x += 1
        y = math.sqrt(math.pow(x, 2) - n)
        if y.is_integer():
            return x - int(y), x + int(y)
        elif x == ((n + 1) / 2):
            return 1, n

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    

