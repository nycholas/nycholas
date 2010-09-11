VPATH += $$PWD

INCLUDEPATH += \
    $$PWD \
    $$PWD/models \

DEPENDPATH += \
    $$PWD \
    $$PWD/models \
    $$PWD/widgets \

# Input
HEADERS += \
    $$PWD/changepassword.h \
    $$PWD/contenttypes.h \
    $$PWD/contenttypesform.h \
    $$PWD/models/contenttypesmodel.h \
    $$PWD/contenttypessearch.h \
    $$PWD/group.h \
    $$PWD/groupform.h \
    $$PWD/models/groupmodel.h \
    $$PWD/groupsearch.h \
    $$PWD/login.h \
    $$PWD/mainwindow.h \
    $$PWD/permission.h \
    $$PWD/permissionform.h \
    $$PWD/models/permissionmodel.h \
    $$PWD/permissionsearch.h \
    $$PWD/user.h \
    $$PWD/userform.h \
    $$PWD/models/usermodel.h \
    $$PWD/usersearch.h \
#    $$PWD/xxx.h \
#    $$PWD/xxxform.h \
#    $$PWD/models/xxxmodel.h \
#    $$PWD/xxxsearch.h \

SOURCES += \
    $$PWD/changepassword.cpp \
    $$PWD/contenttypes.cpp \
    $$PWD/contenttypesform.cpp \
    $$PWD/models/contenttypesmodel.cpp \
    $$PWD/contenttypessearch.cpp \
    $$PWD/group.cpp \
    $$PWD/groupform.cpp \
    $$PWD/models/groupmodel.cpp \
    $$PWD/groupsearch.cpp \
    $$PWD/login.cpp \
    $$PWD/mainwindow.cpp \
    $$PWD/permission.cpp \
    $$PWD/permissionform.cpp \
    $$PWD/models/permissionmodel.cpp \
    $$PWD/permissionsearch.cpp \
    $$PWD/user.cpp \
    $$PWD/userform.cpp \
    $$PWD/models/usermodel.cpp \
    $$PWD/usersearch.cpp \
#    $$PWD/xxx.cpp \
#    $$PWD/xxxform.cpp \
#    $$PWD/models/xxxmodel.cpp \
#    $$PWD/xxxsearch.cpp \

FORMS += \
    $$PWD/widgets/changepassword.ui \
    $$PWD/widgets/contenttypes.ui \
    $$PWD/widgets/contenttypesform.ui \
    $$PWD/widgets/contenttypessearch.ui \
    $$PWD/widgets/group.ui \
    $$PWD/widgets/groupform.ui \
    $$PWD/widgets/groupsearch.ui \
    $$PWD/widgets/login.ui \
    $$PWD/widgets/mainwindow.ui \
    $$PWD/widgets/permission.ui \
    $$PWD/widgets/permissionform.ui \
    $$PWD/widgets/permissionsearch.ui \
    $$PWD/widgets/user.ui \
    $$PWD/widgets/userform.ui \
    $$PWD/widgets/usersearch.ui \
#    $$PWD/widgets/xxx.ui \
#    $$PWD/widgets/xxxform.ui \
#    $$PWD/widgets/xxxsearch.ui \ 

RESOURCES += \
    $$PWD/widgets/resources.qrc \