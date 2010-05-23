#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Example select in oracle data base.
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
import cx_Oracle

conn = cx_Oracle.connect('dbuser', 'dbpasswd', 'localhost:1521/DATABASE')
curs = conn.cursor()
curs.execute("""
    select col_1, col_2, col_3
    from tb_test
    where col_1 = :arg_1
        and arg_1 = :arg_2
        and arg_2 >= 666""",
    arg_1 = "cenobites.hell",
    arg_2 = 666)
for col_1, col_2, col_3 in curs.fecthall():
    print "Values: %s, %s, %s" % (col_1, col_2, col_3)
curs.close()
conn.close()
