# -*- coding: utf-8 -*-
#
# eCrawler - E-mail Crawler.
# Copyright (C) 2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
#
# This file is part of eCrawler.
#
# eCrawler is free software: you can redistribute it and/or modify
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
import logging
from email.Header import decode_header
from unicodedata import normalize

def str_normalizer(st, coding):
    """Description here.
    
    >>> str_normalizer('', 'utf-8')
    ''
    """
    logging.debug("In str_normalizer()")
    try:
        return normalize('NFKD', st.decode(coding)).encode('ascii', 'ignore')
    except UnicodeDecodeError: # Ex.: us-ascii (ć)
        return st
    except LookupError: # Ex.: unicode-1-1-ascii
        return st

def mail_str_normalizer(st):
    """Description here.
    
    >>> mail_str_normalizer('')
    ['']
    """
    logging.debug("In mail_str_normalizer()")
    try:
        lst_st = decode_header(st)
    except UnicodeEncodeError: # Ex.: Solicitação Cartão Valecard_Sirineu.docx
        lst_st = [(st, None)]
    lst_out = []
    for s in lst_st:
        if not s[1] is None:
            lst_out.append(str_normalizer(*s))
        else:
            lst_out.append(s[0])
    return lst_out

def mail_filename_normalizer(st):
    """Description here.
    
    >>> mail_filename_normalizer('')
    ''
    """
    logging.debug("In mail_filename_normalizer()")
    st = urllib.unquote(st)
    st = re.sub(r'filename\*.?[0-9].?\=', '', st)
    st = ''.join(mail_str_normalizer(st))
    return st.replace(' ', '_').replace('/', '_').replace(';', '')

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
    logging.debug("In no_tags()")
    return re.sub(r'<.*?>', repl, str(st))


if __name__ == "__main__":
    import doctest
    doctest.testmod()