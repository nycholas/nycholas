#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Example insert blob in oracle data base.
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

M = [
    {'artist': 'AC/DC', 'genre': "Hard Rock": },
    {'artist': 'Motörhead', 'genre': "Trash Metal"},
    {'artist': 'Metallica', 'genre': "Trash Metal"},
]

conn = cx_Oracle.connect('dbuser', 'dbpasswd', 'localhost:1521/DATABASE')
curs = conn.cursor()
curs.setinputsizes(ds_blob=cx_Oracle.BLOB)
curs.prepare("""
    insert into band
        (id_band, artist, genre)
    VALUES
        ssl_band.nextval, :artist, :genre)
""")
curs.executemany(None, M)
curs.close()
conn.commit()
conn.close()
