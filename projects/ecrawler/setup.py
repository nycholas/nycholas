#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# eCrawler - E-mail Crawler.
# Copyright (C) 2009-2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
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
import sys
import locale
from distutils.core import setup

locale.setlocale(locale.LC_ALL, "")
try:
    sys.setappdefaultencoding("utf-8")
except AttributeError:
    try:
        reload(sys)
        sys.setdefaultencoding("utf-8")
    except LookupError:
        pass

long_description = """\
eCrawler - E-mail Crawler.
"""

def scripts():
    path = os.path.join(os.curdir, "bin")
    filename_ecrawler = "%s%secrawler" % (path, os.sep)
    filename_get_email_by_id = "%s%sget_email_by_id" % (path, os.sep)
    if os.path.exists(filename_ecrawler):
        os.remove(filename_ecrawler)
    os.link("ecrawler.py", filename_ecrawler)
    return [filename_ecrawler, filename_get_email_by_id,]

def data_files():
    data_files = []
    data_path_src = os.curdir
    data_path_dst = os.curdir
    data_files.append((data_path_src, ["AUTHORS", "ChangeLog", "CONTRIBUTORS", 
                                   "COPYING", "COPYING.LESSER", "FAQ", 
                                   "INSTALL", "README", "THANKS", "TODO",]))
    data_path_src = os.path.join("ecrawler", "resources")
    data_path_dst = os.path.join(os.path.expanduser("~"), ".ecrawler", "resources")
    data_files.append((data_path_dst,
                      [os.path.join(data_path_src, "forward.conf"), 
                       os.path.join(data_path_src, "logging.conf"), 
                       os.path.join(data_path_src, "profile.conf"), 
                       os.path.join(data_path_src, "profile.conf.sample"),]))
    data_path_src = os.path.join("ecrawler", "resources")
    data_path_dst = os.path.join("share", "ecrawler", "resources")
    data_files.append((data_path_dst,
                      [os.path.join(data_path_src, "forward.conf"), 
                       os.path.join(data_path_src, "logging.conf"), 
                       os.path.join(data_path_src, "profile.conf"), 
                       os.path.join(data_path_src, "profile.conf.sample"),]))
    data_path_src = os.curdir
    data_path_dst = os.path.join("share", "ecrawler")
    data_files.append((data_path_dst,
                      ["AUTHORS", "ChangeLog", "CONTRIBUTORS", "COPYING",
                       "COPYING.LESSER", "FAQ", "INSTALL", "README", 
                       "THANKS", "TODO",]))
    data_path_src = os.path.join("ecrawler", "share", "log")
    data_path_dst = os.path.join(os.path.expanduser("~"), ".ecrawler", "log")
    data_files.append((data_path_dst,
                      []))
    locale = os.path.join("share", "locale")
    try:
        langs = [i for i in os.listdir(locale) \
                 if os.path.isdir(os.path.join(locale, i))]
    except OSError:
        langs = []
    for lang in langs:
        listFiles = []
        diretory = os.path.join("share", "ecrawler",
                                "locale", lang, "LC_MESSAGES")
        mo = os.path.join("share", "locale", lang,
                          "LC_MESSAGES", "ecrawler.mo")
        if os.path.isfile(mo):
           listFiles.append(mo)
        data_files.append((diretory, listFiles))
    return data_files

def run():
    setup(name="eCrawler",
          version="0.1",
          author="Nycholas de Oliveira e Oliveira",
          author_email="nycholas@gmail.com",
          url="http://code.google.com/p/nycholas/source/browse/#hg/projects/ecrawler",
          download_url="http://code.google.com/p/nycholas/source/browse/#hg/projects/ecrawler",
          license="GNU Lesser General Public License (LGPL)",
          description="""eCrawler - E-mail Crawler.""",
          long_description=long_description,
          platforms=["Many"],
          packages=["ecrawler", "ecrawler.forwards", "ecrawler.forwards.app", 
                    "ecrawler.forwards.db", "ecrawler.utils",],
          scripts=scripts(),
          data_files=data_files()
          )

# Commands:
# ./setup.py clean -a
# ./setup.py install -c -O2
# ./setup.py sdist --formats=bztar, gztar, tar, zip, ztar
# ./setup.py bdist --formats=rpm, gztar, bztar, ztar, tar, wininst, zip
if __name__ == "__main__":
    run()
