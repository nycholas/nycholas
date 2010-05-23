#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Pizza Steiner, the largest number of pieces with no cuts.
# Copyright (C) 2009 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import math

def pizza_steiner(n):
    """Pizza Steiner, the largest number of pieces with no cuts.

    >>> [pizza_steiner(n) for n in range(10)]
    [1, 2, 4, 7, 11, 16, 22, 29, 37, 46]
    >>> [pizza_steiner(long(n)) for n in range(10)]
    [1, 2, 4, 7, 11, 16, 22, 29, 37, 46]
    >>> pizza_steiner(666)
    222112
    >>> pizza_steiner(666L)
    222112
    >>> pizza_steiner(-1)
    Traceback (most recent call last):
        ...
    ValueError: n must be >= 0

    >>> pizza_steiner(666.6)
    Traceback (most recent call last):
        ...
    ValueError: n must be exact integer

    >>> pizza_steiner(1e100)
    Traceback (most recent call last):
        ...
    OverflowError: n too large
    """
    if not n >= 0:
        raise ValueError("n must be >= 0")
    if math.floor(n) != n:
        raise ValueError("n must be exact integer")
    if n + 1 == n:
        raise OverflowError("n too large")
    return sum(range(n + 1)) + 1


if __name__ == "__main__":
    import doctest
    doctest.testmod()
