# -*- coding: utf-8 -*-
#
# ask-undrgz system of questions uses data from underguiz.
# Copyright (c) 2010, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# # Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# # Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# # Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
#    its contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
import re
import unicodedata

from google.appengine.ext import db


class SlugProperty(db.StringProperty):
    
    """A (rough) App Engine equivalent to Django's SlugField."""

    re_strip = re.compile(r'[^\w\s-]')
    re_dashify = re.compile(r'[-\s]+')

    def __init__(self, auto_calculate, **kwargs):
        """Initialize a slug with the property to base it on.
        
        The property passed in for auto_calculate should be the property object
        itself from the model::

          class Spam(BaseModel):
            name = db.StringProperty()
            slug = SlugProperty(name)

        """
        super(SlugProperty, self).__init__(**kwargs)
        self.auto_calculate = auto_calculate

    @classmethod
    def slugify_(yo_mama, value):
        cleanup = yo_mama.re_strip.sub('', value).strip().lower()
        return yo_mama.re_dashify.sub('-', cleanup)
    
    @classmethod
    def slugify(yo_mama, value):
        """
        Normalizes string, converts to lowercase, removes non-alpha characters,
        and converts spaces to hyphens.
        """
        value = unicodedata.normalize('NFKD', value).encode('ascii', 'ignore')
        value = unicode(re.sub('[^\w\s-]', '', value).strip().lower())
        return re.sub('[-\s]+', '-', value)

    def default_value(self):
        """Cannot calculate a default value because of a lack of details for
        use with the descriptor."""
        return super(SlugProperty, self).default_value()

    def get_value_for_datastore(self, model_instance):
        """Convert slug into format to go into the datastore."""
        value = self.auto_calculate.__get__(model_instance, None)
        return self.slugify(value)

    def validate(self, value):
        """Validate the slug meets formatting restrictions."""
        # Django does [^\w\s-] to '', strips, lowers, then [-\s] to '-'.
        #if value and (value.lower() != value or ' ' in value):
        #    raise db.BadValueError("%r must be lowercase and have no spaces" %
        #                           value)
        return super(SlugProperty, self).validate(value)
    
    