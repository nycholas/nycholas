#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def gcd(a, b):
    """http://en.wikipedia.org/wiki/Euclidean_algorithm

    >>> gcd(1234, 54)
    2
    >>> gcd(5324, 5324)
    5324
    >>> gcd(234, 0)
    234
    >>> gcd(0, 234)
    Traceback (most recent call last):
        ....
    ValueError: a must be > 0
    >>> gcd(14, 35)
    7
    >>> gcd(252, 180)
    36
    >>> gcd(6643, 2873)
    13
    >>> gcd(272828282, 3242)
    2
    """
    if a == 0:
        raise ValueError('a must be > 0')
    elif b == 0:
        return a
    while a != b:
        if a > b:
            a -= b
        else:
            b -= a
    return a

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    

