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
import os
import re
import glob
import urllib
import smtplib
import logging
from email.Header import decode_header
from unicodedata import normalize

def str_normalizer(st, coding):
    """Description here.
    
    >>> str_normalizer('', 'utf-8')
    ''
    """
    logging.debug("In commons.str_normalizer()")
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
    logging.debug("In commons.mail_str_normalizer()")
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
    logging.debug("In commons.mail_filename_normalizer()")
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
    logging.debug("In commons.no_tags()")
    return re.sub(r'<.*?>', repl, str(st))

def search_files(dirpath):
    logging.debug("In commons.search_files()")
    if not os.path.exists(dirpath):
        return []
    logging.info("Search file in %s..." % dirpath)
    files_zip = glob.glob("%s%s%s" % (dirpath, os.path.sep, "*"))
    logging.debug(":: Files in directory %s" % files_zip)
    if not files_zip:
        logging.info("None files in directory %s" % dirpath)
        return []
    return files_zip

def search_zipfile(dirpath):
    logging.debug("In commons.search_zipfile()")
    filedir = os.path.join(dirpath, os.path.pardir)
    if not os.path.exists(filedir):
        return ""
    logging.info("Search file in %s..." % filedir)
    files_zip = glob.glob("%s%s%s" % (filedir, os.path.sep, "*.zip"))
    logging.debug(":: Files in directory %s" % files_zip)
    if not files_zip:
        logging.info("None files in directory %s" % filedir)
        return ""
    file_zip = files_zip[-1]
    return file_zip

def files_to_str(dirpath):
    logging.debug("In commons.files_to_str()")
    lst_st = []
    if os.path.exists(dirpath):
        for dirpath, dirnames, filenames in os.walk(dirpath):
            for filename in filenames:
                filepath = os.path.join(dirpath, filename)
                lst_st.append(open(filepath).read())
    return "".join(lst_st)

def file_to_bin(filepath):
    logging.debug("In commons.file_to_bin()")
    if not os.path.exists(filepath):
        return ""
    return open(filepath, "rb").read()

def send_email(hostname, port, username, password, from_addr, to_addrs, msg):
    logging.debug("In commons.send_email()")
    server = smtplib.SMTP(hostname, port)
    server.ehlo()
    if username and password:
        server.starttls()
        server.login(username, password)
    server.sendmail(from_addr, to_addrs, msg)


if __name__ == "__main__":
    import doctest
    doctest.testmod()