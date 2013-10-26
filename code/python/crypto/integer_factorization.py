#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def factor(n):
    """
    >>> factor(2)
    2
    >>> factor(223432)
    2
    >>> factor(223433)
    7
    >>> factor(0)
    Traceback (most recent call last):
      ...
    ValueError: n must be > 0
    >>> factor(13)
    13
    >>> factor(620234234891)
    127
    >>> factor(1)
    1
    >>> factor(293239)
    83
    """
    if n <= 0:
        raise ValueError('n must be > 0')
    F = 2
    q, r = divmod(n, F)
    while r != 0:
        F += 1
        if F > (n**0.5):
            return n
        q, r = divmod(n, F)
    return F

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    

