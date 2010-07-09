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
import glob
import time
import email
import socket
import imaplib
import zipfile
import logging
import traceback
import threading
import mimetypes

from error import *
from utils.commons import *
from models import EmailModel
from middleman import Destiny


class Crawler(threading.Thread):
    
    def __init__(self, id, emails, profile):
        logging.debug("In Crawler::__init__(%d)" % id)
        super(Crawler, self).__init__()
        self.emails = emails
        self.id = id
        self.profile = profile.get("profile")
        self.hostname = profile.get("hostname") 
        self.port = profile.get("port")
        self.username = profile.get("username")
        self.password = profile.get("password")
        self.mailbox = profile.get("mailbox")
        self.directory = profile.get("directory")
        self.smtp_hostname = profile.get("smtp_hostname")
        self.smtp_port = profile.get("smtp_port")
        self.smtp_username = profile.get("smtp_username")
        self.smtp_password = profile.get("smtp_password")
        self.smtp_from_addr = profile.get("smtp_from_addr")
        self.smtp_to_addrs = profile.get("smtp_to_addrs")
        self.smtp_prefix = profile.get("smtp_prefix")
        self.forwards = profile.get("forwards")
        self.is_remove = profile.get("is_remove")
        self.is_test = profile.get("is_test")
        self._email_id_errors = []
        
    def run(self):
        logging.debug("In Crawler::run(%d)" % self.id)
        logging.debug(":: %d-emails(%d): %s" % (self.id, len(self.emails),
                                                self.emails))

        logging.info("%d-Connecting in %s:%d..." % (self.id, self.hostname, 
                                                    self.port))
        try:
            self._m = imaplib.IMAP4_SSL(self.hostname, self.port)
        except (socket.gaierror, socket.error), e:
                logging.error("!! %d-Error: %s [%s:%s]" % (self.id, str(e), 
                              self.hostname, self.port))
                logging.error("Traceback:", exc_info=1)
                raise ConnectionError, e

        logging.info("%d-Authenticating with %s..." % (self.id, self.username))
        try:
            self._m.login(self.username, self.password)
        except imaplib.IMAP4_SSL.error, e:
                logging.error("!! %d-Error: %s [%s@%s:%s]" % (self.id, str(e), 
                              self.username, self.hostname, self.port))
                logging.error("Traceback:", exc_info=1)
                raise ConnectionError, e

        logging.info("%d-Select a mailbox..." % self.id)
        self._m.select()

        email_models = []
        for email_id in self.emails:
            logging.debug(":: %d-email_id: %s" % (self.id, email_id))
            try:
                resp, data = self._m.fetch(email_id, "(RFC822)")
                email_body = data[0][1]
            except (TypeError, imaplib.IMAP4_SSL.error), e:
                logging.error("!! %d-Error: %s [email_id: %s]" % \
                              (self.id, str(e), email_id))
                logging.error("Traceback:", exc_info=1)
                self.add_email_error(email_id)
                continue

            try:
                msg = email.message_from_string(email_body)
            except (email.Errors.MessageParseError, 
                    email.Errors.HeaderParseError), e:
                logging.error("!! %d-Error: %s [email_id: %s]" % \
                              (self.id, str(e), email_id))
                logging.error("Traceback:", exc_info=1)
                self.add_email_error(email_id)
                continue
            
            msg_from = self.get_mail_address(msg["From"])
            msg_subject = self.get_mail_subject(msg["Subject"])
            msg_to = self.get_mail_address(msg["To"])
            msg_date = self.get_mail_datetime(msg["Date"])

            logging.debug(":: %d-from: %s" % (self.id, msg_from))
            logging.debug(":: %d-subject: %s" % (self.id, msg_subject))
            logging.debug(":: %d-to: %s" % (self.id, msg_to))
            logging.debug(":: %d-date: %s" % (self.id, msg_date))

            logging.info("%d-Create directories..." % self.id)
            directories = [self.mkdir(email_id, i) for i in ("content", "annex")]
            (content_dir, annex_dir) = directories
            logging.debug("%d-Directories creates: %s" % (self.id, directories))
            
            logging.info("%d-Reading email..." % self.id)
            is_content_text = False
            counter = 1
            for part in msg.walk():
                filename = part.get_filename()
                ext = ".bin"
                if not filename:
                    ext = mimetypes.guess_extension(part.get_content_type())
                    if not ext:
                        ext = ".bin"
                    filename = "part-%03d%s" % (counter, ext)
                filename = self.get_mail_filename(filename)
                logging.info("%d-Filename: %s" % (self.id, filename))

                if part.is_multipart():
                    logging.info("%d-Is multipart files" % self.id)
                    continue

                # Content text/plain
                dtypes = part.get_params(None, "Content-Disposition")
                if not dtypes:
                    logging.info("%d-Any parameter in the mail" % self.id)
                    if part.get_content_type() == "text/plain":
                        logging.info("%d-Content disposition: text/plain" % self.id)
                        charset = part.get_content_charset()
                        payload = part.get_payload(decode=True)
                        path = os.path.join(content_dir, filename)
                        logging.debug(":: %d-Charset: %s" % (self.id, charset))
                        logging.debug(":: %d-Save content in: %s" % (self.id, 
                                                                     path))
                        content = self.get_mail_content(payload, charset)
                        fp = open(path, "wb")
                        fp.write(content)
                        fp.close()
                        if content.strip() != "":
                            logging.info("%d-Make email '\\Seen'" % self.id)
                            self._m.store(email_id, "+FLAGS", "\\Seen")
                            is_content_text = True
                            continue

                    if not is_content_text and part.get_content_type() == "text/html":
                        logging.info("%d-Content disposition: text/html" % self.id)
                        charset = part.get_content_charset()
                        payload = part.get_payload(decode=True)
                        path = os.path.join(content_dir, filename)
                        logging.debug(":: %d-Charset: %s" % (self.id, charset))
                        logging.debug(":: %d-Save content in: %s" % (self.id, 
                                                                     path))
                        content = self.get_mail_content_html(payload, charset)
                        fp = open(path, "wb")
                        fp.write(content)
                        fp.close()
                        if content.strip() != "":
                            logging.info("%d-Make email '\\Seen'" % self.id)
                            self._m.store(email_id, "+FLAGS", "\\Seen")
                            is_content_text = True
                            continue
                    ctypes = part.get_params()
                    if not ctypes:
                        continue
                    for key, val in ctypes:
                        logging.debug(":: %d-Params: %s = %s" % (self.id,
                                                                 key, val))
                        key = key.lower()
                        if key == "name":
                            filename = val
                            break
                    else:
                        continue
                else:
                    logging.info("%d-Parameter in the mail" % self.id)
                    attachment, filename = None, None
                    for key, val in dtypes:
                        logging.debug(":: %d-Params: %s = %s" % (self.id,
                                                                 key, val))
                        key = key.lower()
                        if key == 'inline':
                            logging.info("Found content inline...")
                            filename = part.get_filename()
                            ext = ".bin"
                            if not filename:
                                ext = mimetypes.guess_extension(
                                                    part.get_content_type())
                                if not ext:
                                    ext = ".bin"
                                filename = "part-%03d%s" % (counter, ext)
                            filename = self.get_mail_filename(filename)
                            logging.info("%d-Filename: %s" % (self.id, filename))

                            logging.info("%d-Any parameter in the mail" % self.id)
                            if part.get_content_type() == "text/plain":
                                logging.info("%d-Content disposition: text/plain" % self.id)
                                charset = part.get_content_charset()
                                payload = part.get_payload(decode=True)
                                path = os.path.join(content_dir, filename)
                                logging.debug(":: %d-Charset: %s" % (self.id, 
                                                                     charset))
                                logging.debug(":: %d-Save content in: %s" % (self.id, path))
                                content = self.get_mail_content(payload, charset)
                                fp = open(path, "wb")
                                fp.write(content)
                                fp.close()
                                if content.strip() != "":
                                    logging.info("%d-Make email '\\Seen'" % self.id)
                                    self._m.store(email_id, "+FLAGS", "\\Seen")
                                    is_content_text = True
                                    continue

                            if not is_content_text and part.get_content_type() == "text/html":
                                logging.info("%d-Content disposition: text/html" % self.id)
                                charset = part.get_content_charset()
                                payload = part.get_payload(decode=True)
                                path = os.path.join(content_dir, filename)
                                logging.debug(":: %d-Charset: %s" % (self.id, 
                                                                     charset))
                                logging.debug(":: %d-Save content in: %s" % (self.id, path))
                                content = self.get_mail_content_html(payload, 
                                                                     charset)
                                fp = open(path, "wb")
                                fp.write(content)
                                fp.close()
                                if content.strip() != "":
                                    logging.info("%d-Make email '\\Seen'" % self.id)
                                    self._m.store(email_id, "+FLAGS", "\\Seen")
                                    is_content_text = True
                                    continue
                        if key == "filename":
                            logging.info("Found content filename...")
                            filename = val
                            attachment = True
                        if key == "attachment":
                            logging.info("Found content attachment...")
                            attachment = True
                    if not attachment:
                        continue

                filename = self.get_mail_filename(filename, ext)
                logging.info("%d-Filename: %s" % (self.id, filename))
                path = os.path.join(annex_dir, filename)
                logging.info("%d-Saving attachment: %s" % (self.id, path))
                fp = open(path, "wb")
                fp.write(part.get_payload(decode=True))
                fp.close()

                logging.info("%d-Make email '\\Seen'" % self.id)
                self._m.store(email_id, "+FLAGS", "\\Seen")

                counter += 1
            logging.info("%d-Permanently remove deleted items..." % self.id)
            self._m.expunge()

            logging.info("%d-Creating zip file..." % self.id)
            zipfile = self.zip_attachment(annex_dir, email_id)
            
            logging.info("%d-Populating email model..." % self.id)
            params = {
                "email_id": email_id, "to": msg_to, "from": msg_from, 
                "subject": msg_subject, "date": msg_date, 
                "content_dir": content_dir, "annex_dir": annex_dir,
            }
            logging.debug(":: params: %s" % str(params))
            
            try:
                email_model = EmailModel(params)
                email_model.populate(self.forwards)
                email_models.append(email_model)
            except ModelError, e:
                logging.error("!! %d-Error: %s [email_id: %s]" % \
                              (self.id, str(e), email_id))
                logging.error("Traceback:", exc_info=1)
                self.add_email_error(email_id)
                continue
        
        try:
            destinations = Destiny(self.forwards.keys())
            destinations.execute(email_models)
            for email_id in destinations.email_errors():
                self.add_email_error(email_id)
        except ForwardError, e:
            logging.error("!! Error: %s" % str(e))
            logging.error("Traceback:", exc_info=1)
        
        if self.is_remove:
            logging.info("%d-Clean directories and files..." % self.id)
            for email_id in self.emails:
                logging.debug("%d-Remove directories and files: %s..." % \
                              (self.id, email_id))
                self.rmdir(email_id)
                #self.add_email_error(email_id) # ;-), Test!
            
        logging.info("%d-Running rollback..." % self.id)
        self.rollback()
            
        logging.info("%d-Close connection..." % self.id)
        self._m.close()

        logging.info("%d-Logout with %s..." % (self.id, self.username))
        self._m.logout()
        
    def add_email_error(self, email_id):
        logging.debug("In Crawler::add_email_error(%d)" % self.id)
        logging.debug("++ %d-email_id: %s" % (self.id, email_id))
        if not email_id in self._email_id_errors: 
            self._email_id_errors.append(email_id)
        
    def rollback(self):
        logging.debug("In Crawler::rollback(%d)" % self.id)        
        if not self._email_id_errors:
            return
        try:
            self._m.check()
        except imaplib.IMAP4_SSL.error, e:
            logging.error("!! Error: %s" % str(e))
            logging.error("Traceback:", exc_info=1)
            raise RollbackError, e
        
        logging.info("%d-Make emails '\\Unseen'" % self.id)
        for email_id in self._email_id_errors:
            logging.debug("%d-Make email '\\Unseen': %s" % (self.id, email_id))
            self._m.store(email_id, "-FLAGS", "\\Seen")
            
        subject = "[%s] %s" % (self.smtp_prefix, 
                               time.strftime("%a, %d %b %Y %H:%M:%S %z (%Z)"))
        msg = """:D, 
        
Were reported some errors when running the collector:
        
    E-mails: %s
    Traceback: %s


--
So long and good luck,
Nycholas de Oliveira e Oliveira.
        """ % (str(self._email_id_errors), traceback.extract_stack())
            
        logging.debug("%d-Send email: %s" % (self.id, 
                                             str(self._email_id_errors)))
        try:
            send_email(self.smtp_hostname, self.smtp_port, 
                   self.smtp_username, self.smtp_password, 
                   self.smtp_from_addr, self.smtp_to_addrs, 
                   subject, msg)
        except Exception, e:
            logging.error("!! Error: %s" % str(e))
            logging.error("Traceback:", exc_info=1)
            try:
                raise RollbackError, e
            finally:
                self._email_id_errors = []
            
        logging.info("%d-Finish rollback" % self.id)
        
    def force_close(self):
        logging.debug("In Crawler::force_close(%d)" % self.id)
        try:
            raise ConnectionError
        finally:
            logging.info("%d-Force close connection..." % self.id)
            self._m.close()
    
            logging.info("%d-Force logout with %s..." % (self.id, self.username))
            self._m.logout()

    def mkdir(self, email_id, directory):
        logging.debug("In Crawler::mkdir(%d)" % self.id)
        logging.debug("++ %d-email_id: %s" % (self.id, email_id))
        logging.debug("++ %d-directory: %s" % (self.id, directory))
        prefix_dir = os.path.join(self.directory, self.profile)
        path = os.path.join(prefix_dir, email_id, directory)
        if not os.path.exists(path):
            logging.info(":: %d-Creating directory: %s" % (self.id, path))
            try:
                os.makedirs(path)
            except OSError, e:
                logging.error("!! Error: %s" % str(e))
                logging.error("Traceback:", exc_info=1)
                raise StructureError, e
        return path
    
    def rmdir(self, email_id):
        logging.debug("In Crawler::rmdir(%d)" % self.id)
        logging.debug("++ %d-email_id: %s" % (self.id, email_id))
        email_dir = os.path.join(self.directory, self.profile, email_id)
        if os.path.exists(email_dir):
            for dirpath, dirnames, filenames in os.walk(email_dir):
                for filename in filenames:
                    path = os.path.join(dirpath, filename)
                    logging.debug(":: %d-Removing file %s..." % (self.id, path))
                    try:
                        os.remove(path)
                    except OSError, e:
                        logging.error("!! Error: %s" % str(e))
                        logging.error("Traceback:", exc_info=1)
                        #raise StructureError, e
                        continue
            for dirpath, dirnames, filenames in os.walk(email_dir):
                for dirname in dirnames:
                    path = os.path.join(dirpath, dirname)
                    logging.debug(":: %d-Removing directory %s..." % (self.id, 
                                                                      path))
                    try:
                        os.removedirs(path)
                    except OSError, e:
                        logging.error("!! Error: %s" % str(e))
                        logging.error("Traceback:", exc_info=1)
                        #raise StructureError, e
                        continue

    def zip_attachment(self, path, email_id):
        logging.debug("In Crawler::zip_attachment(%d)" % self.id)
        logging.debug("++ %d-path: %s" % (self.id, path))
        logging.debug("++ %d-email_id: %s" % (self.id, email_id))
        if not os.path.exists(path):
            logging.info("%d-Directory %s not exists" % (self.id, path))
            return ""

        zip_file = "%s_annex.zip" % email_id
        zip_path = os.path.join(path, os.path.pardir, zip_file)
        files_dir = "%s%s%s" % (path, os.path.sep, "*")
        logging.info("%d-Search file in %s..." % (self.id, files_dir))
        files = glob.glob(files_dir)
        logging.debug(":: %d-Files in directory %s" % (self.id, files))
        if not files:
            logging.info("%d-None files in directory %s" % (self.id, path))
            return ""

        logging.info("%d-Create zip file %s..." % (self.id, zip_path))
        file_zip = zipfile.ZipFile(zip_path, "w")
        for file in files:
            logging.debug(":: %d-Writing %s in zip file" % (self.id, file))
            file_zip.write(file, os.path.basename(file), zipfile.ZIP_DEFLATED)

        logging.info("%d-Close zip file %s..." % (self.id, zip_path))
        file_zip.close()
        return zip_path

    def get_mail_address(self, address):
        logging.debug("In Crawler::get_mail_address(%d)" % self.id)
        str_normalizer = "".join(mail_str_normalizer(address))
        findall = re.findall(r"([\w_.-]+@[\w_.-]+)", str_normalizer)
        if findall:
            return findall[-1]
        return _("(Email not informed)")

    def get_mail_subject(self, st):
        logging.debug("In Crawler::get_mail_subject(%d)" % self.id)
        return "".join(mail_str_normalizer(st))

    def get_mail_datetime(self, date):
        logging.debug("In Crawler::get_mail_datetime(%d)" % self.id)
        date = date or time.strftime("%a, %d %b %Y %H:%M:%S %z (%Z)")
        try:
            date = time.strftime("%d/%m/%Y %H:%M:%S", 
                                 email.Utils.parsedate(date))
        except TypeError: # Ex.: wto cze  1 05:06:09 UTC 2010
            date = time.strftime("%a, %d %b %Y %H:%M:%S %z (%Z)")
        return date

    def get_mail_content(self, text, charset):
        logging.debug("In Crawler::get_mail_content(%d)" % self.id)
        if not charset:
            return text
        return str_normalizer(text, charset)

    def get_mail_content_html(self, text, charset):
        logging.debug("In Crawler::get_mail_content_html(%d)" % self.id)
        text = no_tags(text)
        if not charset:
            return text
        return str_normalizer(text, charset)

    def get_mail_filename(self, filename, ext=""):
        logging.debug("In Crawler::get_mail_filename(%d)" % self.id)
        if not filename:
            return "filename%s"% (ext,)
        if isinstance(filename, tuple) or isinstance(filename, list):
            filename = "".join([fl for fl in filename if not fl is None])
        return mail_filename_normalizer("".join(filename.splitlines()))


if __name__ == "__main__":
    pass