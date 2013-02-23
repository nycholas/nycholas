from django.db import models
from django.core.urlresolvers import reverse


class Notebook(models.Model):
    name = models.CharField(max_length=60)
    description = models.TextField()
    date_joined = models.DateTimeField()
    status = models.BooleanField()

    def __str__(self):
        return '%s' % self.name

    def __unicode__(self):
        return u'%s' % self.name
