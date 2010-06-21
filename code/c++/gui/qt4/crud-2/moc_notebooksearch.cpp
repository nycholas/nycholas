/****************************************************************************
** Meta object code from reading C++ file 'notebooksearch.h'
**
** Created: Thu Jun 17 16:41:01 2010
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "apps/notebooksearch.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'notebooksearch.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_NotebookSearch[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      16,   15,   15,   15, 0x05,
      31,   15,   15,   15, 0x05,

 // slots: signature, parameters, type, tag, flags
      49,   15,   15,   15, 0x08,
      64,   15,   15,   15, 0x08,
      79,   15,   15,   15, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_NotebookSearch[] = {
    "NotebookSearch\0\0formSearched()\0"
    "formSearchClose()\0searchAction()\0"
    "cancelAction()\0closeAction()\0"
};

const QMetaObject NotebookSearch::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_NotebookSearch,
      qt_meta_data_NotebookSearch, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &NotebookSearch::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *NotebookSearch::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *NotebookSearch::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_NotebookSearch))
        return static_cast<void*>(const_cast< NotebookSearch*>(this));
    return QDialog::qt_metacast(_clname);
}

int NotebookSearch::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: formSearched(); break;
        case 1: formSearchClose(); break;
        case 2: searchAction(); break;
        case 3: cancelAction(); break;
        case 4: closeAction(); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void NotebookSearch::formSearched()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void NotebookSearch::formSearchClose()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
