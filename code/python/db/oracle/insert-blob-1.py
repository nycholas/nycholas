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
    {'nm_blob': 'File-001', 'fl_blob': open('/tmp/FILE-001', 'rb').read().encode('hex')},
    {'nm_blob': 'File-002', 'fl_blob': open('/tmp/FILE-002', 'rb').read().encode('hex')},
    {'nm_blob': 'File-003', 'fl_blob': open('/tmp/FILE-003', 'rb').read().encode('hex')},
]

conn = cx_Oracle.connect('dbuser', 'dbpasswd', 'localhost:1521/DATABASE')
curs = conn.cursor()
curs.setinputsizes(ds_blob=cx_Oracle.BLOB)
curs.prepare("""
    insert into t_blob
        (id_blob, nm_blob, fl_blob)
    VALUES
        ssl_t_blob.nextval, :nm_blob, :fl_blob)
""")
curs.executemany(None, M)
curs.close()
conn.commit()
conn.close()
