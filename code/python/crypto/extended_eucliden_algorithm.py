#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def extended_gcd(a, b):
    """http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
    
    >>> extended_gcd(120, 23)
    (-9, 47)
    """
    if a == 0:
        raise ValueError('a must be > 0')
    elif b == 0:
        return a
    x, y, last_x, last_y = 0, 1, 1, 0
    while b != 0:
        q, r = divmod(a, b)
        a, b = b, a % b
        x, last_x = last_x - (q*x), x
        y, last_y = last_y - (q*y), y
    return last_x, last_y

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    

