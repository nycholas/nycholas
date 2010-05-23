#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Get e-mails noseen and attachements by gmail.com.
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
import os
import getpass
import email
import imaplib
import mimetypes

def main():
    host = 'imap.gmail.com'
    port = 993
    user = raw_input('Username: ')
    passwd = getpass.getpass('Password: ')
    detach_dir = './inbox'

    print ' + connect %s:%d...' % (host, port)
    m = imaplib.IMAP4_SSL(host, port)

    print ' + login %s... ' % user
    m.login(user, passwd)

    print ' + select: ALL...'
    m.select()

    print ' + search: ALL...'
    resp, items = m.search(None, 'UNSEEN')
    items = items[0].split()
    items.reverse()

    print ' + nro mails %d...' % len(items)

    for email_id in items:
        print ' : get email %s...' % email_id
        resp, data = m.fetch(email_id, '(RFC822)')
        email_body = data[0][1]

        msg = email.message_from_string(email_body)

        print '  :: from: %s' % msg['From']
        print '  :: subject: %s' % msg['Subject']
        print '  :: to: %s' % msg['To']

        email_dir = os.path.join(detach_dir, email_id)
        if not os.path.exists(email_dir):
            os.makedirs(email_dir)

        content_dir = os.path.join(email_dir, 'content')
        if not os.path.exists(content_dir):
            os.makedirs(content_dir)

        annex_dir = os.path.join(email_dir, 'annex')
        if not os.path.exists(annex_dir):
            os.makedirs(annex_dir)

        counter = 1
        for part in msg.walk():
            filename = part.get_filename()
            if not filename:
                ext = mimetypes.guess_extension(part.get_content_type())
                if not ext:
                    ext = '.bin'
                filename = 'part-%03d%s' % (counter, ext)
            filename = filename.replace('/', '_')

            if part.is_multipart():
                continue

            # Content text/plain
            dtypes = part.get_params(None, 'Content-Disposition')
            if not dtypes:
                if part.get_content_type() == 'text/plain':
                    path = os.path.join(content_dir, filename)
                    print '  :: save content: %s' % path
                    fp = open(path, 'wb')
                    fp.write(part.get_payload(decode=True))
                    fp.close()
                    m.store(email_id, '+FLAGS', '\\Seen')
                    continue

                ctypes = part.get_params()
                if not ctypes:
                    continue
                for key, val in ctypes:
                    print key, val
                    key = key.lower()
                    if key == 'name':
                        filename = val
                        break
                else:
                    continue
            else:
                attachment, filename = None, None
                for key,val in dtypes:
                    key = key.lower()
                    if key == 'filename':
                        filename = val
                    if key == 'attachment':
                        attachment = 1
                if not attachment:
                    continue

            path = os.path.join(annex_dir, filename)
            print '  :: save attachment: %s' % path
            fp = open(path, 'wb')
            fp.write(part.get_payload(decode=True))
            fp.close()

            m.store(email_id, '+FLAGS', '\\Seen')

            counter += 1

    print ' + close connection...'
    m.close()

    print ' + logout...'
    m.logout()


if __name__ == '__main__':
    main()
