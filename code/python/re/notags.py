#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Find HTML tags and replace using regular expressions.
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
import re
import urllib

def no_tags(st, repl=''):
    """Find HTML tags and replace using regular expressions.

    >>> no_tags('<tag>no tags</tag>')
    'no tags'
    >>> no_tags('')
    ''
    >>> no_tags('<tag attr="val">no tags</tag>')
    'no tags'
    >>> no_tags('<tag attr="val"></tag>')
    ''
    """

    return re.sub(r'<.*?>', repl, str(st))


if __name__ == "__main__":
    import doctest
    doctest.testmod()
