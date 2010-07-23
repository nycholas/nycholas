# -*- coding: utf-8 -*-
from google.appengine.ext import db


class Question(db.Model):
    ask = db.StringProperty(required=True)
    answer = db.StringProperty(multiline=True, required=False)
    created = db.DateTimeProperty(auto_now_add=True)

    def __str__(self):
        return '%s' % self.ask

    def __unicode__(self):
        return u'%s' % self.ask
    
    def get_absolute_url(self):
        if self.key():
            return '/?key=%s' % self.key()
        return '/'
    
    def to_dict(self):
        return dict([(p, unicode(getattr(self, p))) for p in self.properties()])


