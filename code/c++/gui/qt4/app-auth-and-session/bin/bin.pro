include(../a3s.pri)

TEMPLATE = app
TARGET = $$IDE_APP_WRAPPER
OBJECTS_DIR =

PRE_TARGETDEPS = $$PWD/a3s

QMAKE_LINK = cp $$PWD/a3s $@ && : IGNORE REST

QMAKE_CLEAN = $$IDE_APP_WRAPPER

target.path  = /bin
INSTALLS    += target
