######################################################################
# Automatically generated by qmake (2.01a) Sat Jul 10 16:05:50 2010
######################################################################

QT += sql
UI_DIR = apps/widgets
TEMPLATE = app
TARGET = 
DEPENDPATH += . apps commons apps/models apps/widgets resources/ui
INCLUDEPATH += . commons apps apps/widgets apps/models

# Input
HEADERS += apps/notebook.h \
           apps/notebookform.h \
           apps/notebooksearch.h \
           commons/connection.h \
           apps/models/notebookmodel.h
FORMS += resources/ui/notebook.ui \
         resources/ui/notebookform.ui \
         resources/ui/notebooksearch.ui
SOURCES += main.cpp \
           apps/notebook.cpp \
           apps/notebookform.cpp \
           apps/notebooksearch.cpp \
           apps/models/notebookmodel.cpp
RESOURCES += resources/ui/resources.qrc
