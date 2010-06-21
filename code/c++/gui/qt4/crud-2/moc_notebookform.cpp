/****************************************************************************
** Meta object code from reading C++ file 'notebookform.h'
**
** Created: Thu Jun 17 16:40:59 2010
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "apps/notebookform.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'notebookform.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_NotebookForm[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       3,       // signalCount

 // signals: signature, parameters, type, tag, flags
      14,   13,   13,   13, 0x05,
      26,   13,   13,   13, 0x05,
      40,   13,   13,   13, 0x05,

 // slots: signature, parameters, type, tag, flags
      54,   13,   13,   13, 0x08,
      74,   13,   13,   13, 0x08,
      87,   13,   13,   13, 0x08,
     117,   13,   13,   13, 0x08,
     132,   13,   13,   13, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_NotebookForm[] = {
    "NotebookForm\0\0formAdded()\0formChanged()\0"
    "formDeleted()\0timerStatusAction()\0"
    "saveAction()\0saveAndContinueSavingAction()\0"
    "removeAction()\0cancelAction()\0"
};

const QMetaObject NotebookForm::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_NotebookForm,
      qt_meta_data_NotebookForm, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &NotebookForm::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *NotebookForm::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *NotebookForm::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_NotebookForm))
        return static_cast<void*>(const_cast< NotebookForm*>(this));
    return QDialog::qt_metacast(_clname);
}

int NotebookForm::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: formAdded(); break;
        case 1: formChanged(); break;
        case 2: formDeleted(); break;
        case 3: timerStatusAction(); break;
        case 4: saveAction(); break;
        case 5: saveAndContinueSavingAction(); break;
        case 6: removeAction(); break;
        case 7: cancelAction(); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}

// SIGNAL 0
void NotebookForm::formAdded()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void NotebookForm::formChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void NotebookForm::formDeleted()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
