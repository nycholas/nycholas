#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def div(a, b):
    """http://en.wikipedia.org/wiki/Division_algorithm

    >>> div(10, 3)
    (3, 1)
    >>> div(10, -3)
    (-3, 1)
    >>> div(-10, 3)
    (-4, 2)
    >>> div(-10, -3)
    (4, 2)
    >>> div(1, 0)
    Traceback (most recent call last):
        ....
    ValueError: b must be > 0
    >>> div(10, 10)
    (1, 0)
    >>> div(42342134234, 27248979976711202323)
    (0, 42342134234)
    >>> div(272489799767, 42342134234)
    (6, 18436994363)
    """
    if b == 0:
        raise ValueError('b must be > 0')
    elif b < 0:
        Q, R = div(a, -b)
        return -Q, R
    elif a < 0:
        Q, R = div(-a, b)
        if R == 0:
            return -Q, 0
        return -1*(Q + 1), b - R
    # a ≥ 0 and b > 0
    Q, R = 0, a
    while R >= b:
        Q += 1
        R -= b
    return Q, R

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    

