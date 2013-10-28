#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import math


def test(n, accuracy=1):
    """http://en.wikipedia.org/wiki/Miller_test

    >>> test(2)
    'probably prime'
    >>> test(3)
    'probably prime'
    >>> test(4)
    'composite'
    >>> test(132132131232)
    'composite'
    >>> test(31)
    'probably prime'
    >>> test(561)
    'composite'
    """
    if n <= 0:
        raise ValueError('n must be > 0')
    elif n == 2:
        return 'probably prime'
    q, r = divmod(int(list(str(n))[-1]), 2)
    if r == 0:
        return 'composite'
    q, r, k = n - 1, 0, 0
    while r == 0:
        m = q
        q, r = divmod(m, 2)
        k += 1
    for b in range(2, n):
        i = 0
        while i < k:
            _q, r = divmod(math.pow(b, m), n)
            if (i == 0 and r == 1) or \
               (i >= 0 and r == n-1):
                return 'probably prime'
            _q, r = divmod(math.pow(r, 2), n)
            i += 1
        accuracy -= 1
        if accuracy == 0:
            return 'composite'

if __name__ == '__main__':
    import doctest
    doctest.testmod()


