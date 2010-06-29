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
import sys
import locale
import logging

import constant as constant

def locale_dir():
    logging.debug("In i18n.locale_dir()")
    localedir = constant.LOCALE_DIR
    if not os.path.exists(localedir):
        logging.warning("Could not load the file internationalization {0}" \
                        .format(localedir))
        return ""
    return localedir

def gettext_locale(language=None):
    logging.debug("In i18n.gettext_locale()")
    locale.setlocale(locale.LC_ALL, '')
    if constant.USE_I18N:
        try:
            import gettext
            from gettext import gettext as _
            
            # TODO: Resolv directory locale system
            if os.path.exists(constant.LOCALE_DIR):
                path = constant.LOCALE_DIR
                if language is None:
                    lc = locale.getlocale()
                    lc_test = ("%s.%s" % (lc[0], lc[1]), lc[0])
                    language = lc_test[0]
                    for i in lc_test:
                        locale_path = os.path.join(path, i)
                        if os.path.exists(locale_path):
                            language = i
                            break
                else:
                    lc_test = ("%s.UTF8" % language, language)
                    language = lc_test[0]
                    for i in lc_test:
                        locale_path = os.path.join(path, i)
                        if os.path.exists(locale_path):
                            language = i
                            break
            else:
                logging.warning("Could not load the file internationalization")
            gettext.install(constant.LOCALE_DOMAIN, localedir=path,
                            unicode=True, codeset=language)
        except Exception, e:
            logging.warning(str(e))
            import __builtin__
            __builtin__.__dict__["_"] = lambda x: x
    else:
        import __builtin__
        __builtin__.__dict__["_"] = lambda x: x
