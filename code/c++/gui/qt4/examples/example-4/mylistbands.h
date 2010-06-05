#include "ui_mylistbands.h"

class MyListBands : public QWidget, private Ui::MyListBands
{
    Q_OBJECT

public:
    MyListBands(QWidget *parent = 0);
};
