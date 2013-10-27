#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import math

def sieve(n):
    """http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

    >>> sieve(2)
    [2]
    >>> sieve(3)
    [2, 3]
    >>> sieve(5)
    [2, 3, 5]
    >>> sieve(11)
    [2, 3, 5, 7, 11]
    >>> sieve(30)
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
    >>> sieve(35)
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]
    """
    if n <= 0:
        raise ValueError('n must be > 0')
    elif n == 2:
        return [2]
    sqrtn = math.trunc(math.sqrt(n))
    primes = {2*x + 1: True for x in range(1, int((n+1) / 2))}
    primes.update({2: True})
    for i in range(3, sqrtn + 1):
        j = i+2
        if primes.get(i) is None or not primes.get(i):
            continue
        for s in range(3, int(n / i) + 1):
            if primes.get(s * i) is None:
                continue
            primes[s * i] = False
    return [i for i in primes.keys() if primes[i] == True]

if __name__ == '__main__':
    import doctest
    doctest.testmod()


