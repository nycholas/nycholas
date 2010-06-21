/********************************************************************************
** Form generated from reading UI file 'notebook.ui'
**
** Created: Thu Jun 17 16:40:51 2010
**      by: Qt User Interface Compiler version 4.6.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_NOTEBOOK_H
#define UI_NOTEBOOK_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTableView>
#include <QtGui/QToolButton>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Notebook
{
public:
    QGridLayout *gridLayout_2;
    QHBoxLayout *horizontalLayout_2;
    QLabel *titleLabel;
    QSpacerItem *horizontalSpacer;
    QPushButton *newPushButton;
    QLabel *statusLabel;
    QFrame *frame;
    QGridLayout *gridLayout;
    QHBoxLayout *horizontalLayout_3;
    QPushButton *activatePushButton;
    QPushButton *deactivatePushButton;
    QPushButton *removePushButton;
    QSpacerItem *horizontalSpacer_2;
    QToolButton *searchAdvancedToolButton;
    QLineEdit *searchLineEdit;
    QTableView *notebookTableView;
    QHBoxLayout *horizontalLayout;
    QLabel *statusNotebookTableViewLabel;
    QSpacerItem *horizontalSpacer_4;
    QPushButton *latestPushButton;
    QPushButton *nextPushButton;
    QLabel *statusPaginationLabel;
    QPushButton *previousPushButton;
    QPushButton *oldestPushButton;
    QHBoxLayout *horizontalLayout_4;
    QSpacerItem *horizontalSpacer_3;
    QPushButton *closePushButton;

    void setupUi(QWidget *Notebook)
    {
        if (Notebook->objectName().isEmpty())
            Notebook->setObjectName(QString::fromUtf8("Notebook"));
        Notebook->resize(734, 491);
        QIcon icon;
        icon.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/face-devilish.png"), QSize(), QIcon::Normal, QIcon::Off);
        Notebook->setWindowIcon(icon);
        gridLayout_2 = new QGridLayout(Notebook);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        titleLabel = new QLabel(Notebook);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setStyleSheet(QString::fromUtf8("color: rgb(85, 85, 127);\n"
"font: 75 14pt \"DejaVu Sans Mono\";"));

        horizontalLayout_2->addWidget(titleLabel);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);

        newPushButton = new QPushButton(Notebook);
        newPushButton->setObjectName(QString::fromUtf8("newPushButton"));
        QIcon icon1;
        icon1.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/document-new.png"), QSize(), QIcon::Normal, QIcon::Off);
        newPushButton->setIcon(icon1);

        horizontalLayout_2->addWidget(newPushButton);


        gridLayout_2->addLayout(horizontalLayout_2, 0, 0, 1, 1);

        statusLabel = new QLabel(Notebook);
        statusLabel->setObjectName(QString::fromUtf8("statusLabel"));
        statusLabel->setStyleSheet(QString::fromUtf8(""));

        gridLayout_2->addWidget(statusLabel, 1, 0, 1, 1);

        frame = new QFrame(Notebook);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        gridLayout = new QGridLayout(frame);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        activatePushButton = new QPushButton(frame);
        activatePushButton->setObjectName(QString::fromUtf8("activatePushButton"));
        QIcon icon2;
        icon2.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-ok-apply.png"), QSize(), QIcon::Normal, QIcon::Off);
        activatePushButton->setIcon(icon2);

        horizontalLayout_3->addWidget(activatePushButton);

        deactivatePushButton = new QPushButton(frame);
        deactivatePushButton->setObjectName(QString::fromUtf8("deactivatePushButton"));
        QIcon icon3;
        icon3.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-ok.png"), QSize(), QIcon::Normal, QIcon::Off);
        deactivatePushButton->setIcon(icon3);

        horizontalLayout_3->addWidget(deactivatePushButton);

        removePushButton = new QPushButton(frame);
        removePushButton->setObjectName(QString::fromUtf8("removePushButton"));
        QIcon icon4;
        icon4.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/edit-delete.png"), QSize(), QIcon::Normal, QIcon::Off);
        removePushButton->setIcon(icon4);

        horizontalLayout_3->addWidget(removePushButton);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_2);

        searchAdvancedToolButton = new QToolButton(frame);
        searchAdvancedToolButton->setObjectName(QString::fromUtf8("searchAdvancedToolButton"));
        QIcon icon5;
        icon5.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/page-zoom.png"), QSize(), QIcon::Normal, QIcon::Off);
        searchAdvancedToolButton->setIcon(icon5);
        searchAdvancedToolButton->setCheckable(true);

        horizontalLayout_3->addWidget(searchAdvancedToolButton);

        searchLineEdit = new QLineEdit(frame);
        searchLineEdit->setObjectName(QString::fromUtf8("searchLineEdit"));
        searchLineEdit->setMinimumSize(QSize(200, 0));
        searchLineEdit->setStyleSheet(QString::fromUtf8("color: rgb(170, 170, 255); \n"
"font: oblique 8pt \"DejaVu Sans Mono\";"));
        searchLineEdit->setDragEnabled(true);

        horizontalLayout_3->addWidget(searchLineEdit);


        gridLayout->addLayout(horizontalLayout_3, 0, 0, 1, 1);

        notebookTableView = new QTableView(frame);
        notebookTableView->setObjectName(QString::fromUtf8("notebookTableView"));
        notebookTableView->setAlternatingRowColors(true);
        notebookTableView->setSortingEnabled(true);

        gridLayout->addWidget(notebookTableView, 1, 0, 1, 1);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        statusNotebookTableViewLabel = new QLabel(frame);
        statusNotebookTableViewLabel->setObjectName(QString::fromUtf8("statusNotebookTableViewLabel"));
        statusNotebookTableViewLabel->setStyleSheet(QString::fromUtf8("color: rgb(85, 85, 255);"));

        horizontalLayout->addWidget(statusNotebookTableViewLabel);

        horizontalSpacer_4 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_4);

        latestPushButton = new QPushButton(frame);
        latestPushButton->setObjectName(QString::fromUtf8("latestPushButton"));
        QIcon icon6;
        icon6.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/go-previous-view.png"), QSize(), QIcon::Normal, QIcon::Off);
        latestPushButton->setIcon(icon6);

        horizontalLayout->addWidget(latestPushButton);

        nextPushButton = new QPushButton(frame);
        nextPushButton->setObjectName(QString::fromUtf8("nextPushButton"));
        QIcon icon7;
        icon7.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/go-previous.png"), QSize(), QIcon::Normal, QIcon::Off);
        nextPushButton->setIcon(icon7);

        horizontalLayout->addWidget(nextPushButton);

        statusPaginationLabel = new QLabel(frame);
        statusPaginationLabel->setObjectName(QString::fromUtf8("statusPaginationLabel"));

        horizontalLayout->addWidget(statusPaginationLabel);

        previousPushButton = new QPushButton(frame);
        previousPushButton->setObjectName(QString::fromUtf8("previousPushButton"));
        QIcon icon8;
        icon8.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/go-next.png"), QSize(), QIcon::Normal, QIcon::Off);
        previousPushButton->setIcon(icon8);

        horizontalLayout->addWidget(previousPushButton);

        oldestPushButton = new QPushButton(frame);
        oldestPushButton->setObjectName(QString::fromUtf8("oldestPushButton"));
        QIcon icon9;
        icon9.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/go-next-view.png"), QSize(), QIcon::Normal, QIcon::Off);
        oldestPushButton->setIcon(icon9);

        horizontalLayout->addWidget(oldestPushButton);


        gridLayout->addLayout(horizontalLayout, 2, 0, 1, 1);


        gridLayout_2->addWidget(frame, 2, 0, 1, 1);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalSpacer_3);

        closePushButton = new QPushButton(Notebook);
        closePushButton->setObjectName(QString::fromUtf8("closePushButton"));
        QIcon icon10;
        icon10.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/window-close.png"), QSize(), QIcon::Normal, QIcon::Off);
        closePushButton->setIcon(icon10);

        horizontalLayout_4->addWidget(closePushButton);


        gridLayout_2->addLayout(horizontalLayout_4, 3, 0, 1, 1);

        QWidget::setTabOrder(newPushButton, activatePushButton);
        QWidget::setTabOrder(activatePushButton, deactivatePushButton);
        QWidget::setTabOrder(deactivatePushButton, removePushButton);
        QWidget::setTabOrder(removePushButton, searchLineEdit);
        QWidget::setTabOrder(searchLineEdit, searchAdvancedToolButton);
        QWidget::setTabOrder(searchAdvancedToolButton, notebookTableView);
        QWidget::setTabOrder(notebookTableView, closePushButton);

        retranslateUi(Notebook);

        QMetaObject::connectSlotsByName(Notebook);
    } // setupUi

    void retranslateUi(QWidget *Notebook)
    {
        Notebook->setWindowTitle(QApplication::translate("Notebook", "Notebook", 0, QApplication::UnicodeUTF8));
        titleLabel->setText(QApplication::translate("Notebook", "Select notebook to change", 0, QApplication::UnicodeUTF8));
        newPushButton->setText(QApplication::translate("Notebook", "&Add notebook", 0, QApplication::UnicodeUTF8));
        newPushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+N", 0, QApplication::UnicodeUTF8));
        statusLabel->setText(QString());
        activatePushButton->setText(QApplication::translate("Notebook", "&Activate", 0, QApplication::UnicodeUTF8));
        activatePushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+E", 0, QApplication::UnicodeUTF8));
        deactivatePushButton->setText(QApplication::translate("Notebook", "&Deactivate", 0, QApplication::UnicodeUTF8));
        deactivatePushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+Shift+E", 0, QApplication::UnicodeUTF8));
        removePushButton->setText(QApplication::translate("Notebook", "&Remove", 0, QApplication::UnicodeUTF8));
        removePushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+Del", 0, QApplication::UnicodeUTF8));
        searchAdvancedToolButton->setText(QApplication::translate("Notebook", "Search Advanced", 0, QApplication::UnicodeUTF8));
        searchAdvancedToolButton->setShortcut(QApplication::translate("Notebook", "Ctrl+K", 0, QApplication::UnicodeUTF8));
        statusNotebookTableViewLabel->setText(QApplication::translate("Notebook", "0 notebook", 0, QApplication::UnicodeUTF8));
        latestPushButton->setText(QApplication::translate("Notebook", "&Latest", 0, QApplication::UnicodeUTF8));
        latestPushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+Shift+Left, Ctrl+Shift+PgUp", 0, QApplication::UnicodeUTF8));
        nextPushButton->setText(QApplication::translate("Notebook", "&Next", 0, QApplication::UnicodeUTF8));
        nextPushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+Left, Ctrl+PgUp", 0, QApplication::UnicodeUTF8));
        statusPaginationLabel->setText(QApplication::translate("Notebook", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'DejaVu Sans Mono'; font-size:8pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">1</span> - <span style=\" font-weight:600;\">25</span> de <span style=\" font-weight:600;\">100</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        previousPushButton->setText(QApplication::translate("Notebook", "&Previous", 0, QApplication::UnicodeUTF8));
        previousPushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+Right, Ctrl+PgDown", 0, QApplication::UnicodeUTF8));
        oldestPushButton->setText(QApplication::translate("Notebook", "&Oldest", 0, QApplication::UnicodeUTF8));
        oldestPushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+Shift+Right, Ctrl+Shift+PgDown", 0, QApplication::UnicodeUTF8));
        closePushButton->setText(QApplication::translate("Notebook", "&Close", 0, QApplication::UnicodeUTF8));
        closePushButton->setShortcut(QApplication::translate("Notebook", "Ctrl+W", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class Notebook: public Ui_Notebook {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_NOTEBOOK_H
