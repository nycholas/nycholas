/****************************************************************************
** Meta object code from reading C++ file 'notebook.h'
**
** Created: Thu Jun 17 16:50:59 2010
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "apps/notebook.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'notebook.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Notebook[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
      16,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      10,    9,    9,    9, 0x08,
      30,    9,    9,    9, 0x08,
      42,    9,    9,    9, 0x08,
      59,    9,    9,    9, 0x08,
      79,    9,    9,    9, 0x08,
     102,   94,    9,    9, 0x08,
     134,  129,    9,    9, 0x08,
     167,    9,    9,    9, 0x08,
     187,  181,    9,    9, 0x08,
     228,    9,    9,    9, 0x08,
     244,    9,    9,    9, 0x08,
     257,    9,    9,    9, 0x08,
     274,    9,    9,    9, 0x08,
     289,    9,    9,    9, 0x0a,
     304,    9,    9,    9, 0x0a,
     323,    9,    9,    9, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Notebook[] = {
    "Notebook\0\0timerStatusAction()\0newAction()\0"
    "activateAction()\0desactivateAction()\0"
    "removeAction()\0checked\0"
    "searchAdvancedAction(bool)\0text\0"
    "searchTextChangedAction(QString)\0"
    "closeAction()\0index\0"
    "doubleClickedItemViewAction(QModelIndex)\0"
    "lastestAction()\0nextAction()\0"
    "previousAction()\0oldestAction()\0"
    "updateModels()\0updateSearchForm()\0"
    "updateSearchFormClose()\0"
};

const QMetaObject Notebook::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_Notebook,
      qt_meta_data_Notebook, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Notebook::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Notebook::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Notebook::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Notebook))
        return static_cast<void*>(const_cast< Notebook*>(this));
    return QWidget::qt_metacast(_clname);
}

int Notebook::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: timerStatusAction(); break;
        case 1: newAction(); break;
        case 2: activateAction(); break;
        case 3: desactivateAction(); break;
        case 4: removeAction(); break;
        case 5: searchAdvancedAction((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 6: searchTextChangedAction((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 7: closeAction(); break;
        case 8: doubleClickedItemViewAction((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 9: lastestAction(); break;
        case 10: nextAction(); break;
        case 11: previousAction(); break;
        case 12: oldestAction(); break;
        case 13: updateModels(); break;
        case 14: updateSearchForm(); break;
        case 15: updateSearchFormClose(); break;
        default: ;
        }
        _id -= 16;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
