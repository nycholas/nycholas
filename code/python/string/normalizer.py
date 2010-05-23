#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Normalizer strings.
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
from unicodedata import normalize

def str_nfkd(st, encoding='utf-8'):
    """Normalizer strings.

    >>> str_nfkd('áàãâäÉÈẼÊË')
    'aaaaaEEEEE'
    >>> str_nfkd('açucar')
    'acucar'
    """
    return normalize('NFKD', st.decode(encoding)).encode('ascii', 'ignore')


if __name__ == "__main__":
    import doctest
    doctest.testmod()