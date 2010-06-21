/********************************************************************************
** Form generated from reading UI file 'notebooksearch.ui'
**
** Created: Thu Jun 17 16:40:51 2010
**      by: Qt User Interface Compiler version 4.6.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_NOTEBOOKSEARCH_H
#define UI_NOTEBOOKSEARCH_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QDateEdit>
#include <QtGui/QDialog>
#include <QtGui/QFormLayout>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QGroupBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>

QT_BEGIN_NAMESPACE

class Ui_NotebookSearch
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *horizontalLayout_2;
    QLabel *titleLabel;
    QSpacerItem *horizontalSpacer;
    QFrame *frame;
    QGridLayout *gridLayout_2;
    QGroupBox *detailsSearchGroupBox;
    QFormLayout *formLayout;
    QLabel *nameLabel;
    QLineEdit *nameLineEdit;
    QLabel *descriptionLabel;
    QLineEdit *descriptionLineEdit;
    QLabel *dateJoinedLabel;
    QHBoxLayout *horizontalLayout_3;
    QDateEdit *dateJoinedInitDateEdit;
    QLabel *toLabel;
    QDateEdit *dateJoinedEndDateEdit;
    QLabel *isActivedLabel;
    QCheckBox *isActivedCheckBox;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *searchPushButton;
    QPushButton *cancelPushButton;
    QPushButton *closePushButton;

    void setupUi(QDialog *NotebookSearch)
    {
        if (NotebookSearch->objectName().isEmpty())
            NotebookSearch->setObjectName(QString::fromUtf8("NotebookSearch"));
        NotebookSearch->resize(412, 249);
        QIcon icon;
        icon.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/face-devilish.png"), QSize(), QIcon::Normal, QIcon::Off);
        NotebookSearch->setWindowIcon(icon);
        gridLayout = new QGridLayout(NotebookSearch);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        titleLabel = new QLabel(NotebookSearch);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setStyleSheet(QString::fromUtf8("color: rgb(85, 85, 127);\n"
"font: 75 14pt \"DejaVu Sans Mono\";"));

        horizontalLayout_2->addWidget(titleLabel);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        gridLayout->addLayout(horizontalLayout_2, 0, 0, 1, 1);

        frame = new QFrame(NotebookSearch);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        gridLayout_2 = new QGridLayout(frame);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        detailsSearchGroupBox = new QGroupBox(frame);
        detailsSearchGroupBox->setObjectName(QString::fromUtf8("detailsSearchGroupBox"));
        formLayout = new QFormLayout(detailsSearchGroupBox);
        formLayout->setObjectName(QString::fromUtf8("formLayout"));
        nameLabel = new QLabel(detailsSearchGroupBox);
        nameLabel->setObjectName(QString::fromUtf8("nameLabel"));

        formLayout->setWidget(0, QFormLayout::LabelRole, nameLabel);

        nameLineEdit = new QLineEdit(detailsSearchGroupBox);
        nameLineEdit->setObjectName(QString::fromUtf8("nameLineEdit"));

        formLayout->setWidget(0, QFormLayout::FieldRole, nameLineEdit);

        descriptionLabel = new QLabel(detailsSearchGroupBox);
        descriptionLabel->setObjectName(QString::fromUtf8("descriptionLabel"));

        formLayout->setWidget(1, QFormLayout::LabelRole, descriptionLabel);

        descriptionLineEdit = new QLineEdit(detailsSearchGroupBox);
        descriptionLineEdit->setObjectName(QString::fromUtf8("descriptionLineEdit"));

        formLayout->setWidget(1, QFormLayout::FieldRole, descriptionLineEdit);

        dateJoinedLabel = new QLabel(detailsSearchGroupBox);
        dateJoinedLabel->setObjectName(QString::fromUtf8("dateJoinedLabel"));

        formLayout->setWidget(3, QFormLayout::LabelRole, dateJoinedLabel);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        dateJoinedInitDateEdit = new QDateEdit(detailsSearchGroupBox);
        dateJoinedInitDateEdit->setObjectName(QString::fromUtf8("dateJoinedInitDateEdit"));

        horizontalLayout_3->addWidget(dateJoinedInitDateEdit);

        toLabel = new QLabel(detailsSearchGroupBox);
        toLabel->setObjectName(QString::fromUtf8("toLabel"));

        horizontalLayout_3->addWidget(toLabel);

        dateJoinedEndDateEdit = new QDateEdit(detailsSearchGroupBox);
        dateJoinedEndDateEdit->setObjectName(QString::fromUtf8("dateJoinedEndDateEdit"));

        horizontalLayout_3->addWidget(dateJoinedEndDateEdit);


        formLayout->setLayout(3, QFormLayout::FieldRole, horizontalLayout_3);

        isActivedLabel = new QLabel(detailsSearchGroupBox);
        isActivedLabel->setObjectName(QString::fromUtf8("isActivedLabel"));

        formLayout->setWidget(2, QFormLayout::LabelRole, isActivedLabel);

        isActivedCheckBox = new QCheckBox(detailsSearchGroupBox);
        isActivedCheckBox->setObjectName(QString::fromUtf8("isActivedCheckBox"));
        isActivedCheckBox->setChecked(true);

        formLayout->setWidget(2, QFormLayout::FieldRole, isActivedCheckBox);


        gridLayout_2->addWidget(detailsSearchGroupBox, 0, 0, 1, 1);


        gridLayout->addWidget(frame, 1, 0, 1, 1);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);

        searchPushButton = new QPushButton(NotebookSearch);
        searchPushButton->setObjectName(QString::fromUtf8("searchPushButton"));
        QIcon icon1;
        icon1.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-ok-apply.png"), QSize(), QIcon::Normal, QIcon::Off);
        searchPushButton->setIcon(icon1);

        horizontalLayout->addWidget(searchPushButton);

        cancelPushButton = new QPushButton(NotebookSearch);
        cancelPushButton->setObjectName(QString::fromUtf8("cancelPushButton"));
        QIcon icon2;
        icon2.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-cancel.png"), QSize(), QIcon::Normal, QIcon::Off);
        cancelPushButton->setIcon(icon2);

        horizontalLayout->addWidget(cancelPushButton);

        closePushButton = new QPushButton(NotebookSearch);
        closePushButton->setObjectName(QString::fromUtf8("closePushButton"));
        QIcon icon3;
        icon3.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/window-close.png"), QSize(), QIcon::Normal, QIcon::Off);
        closePushButton->setIcon(icon3);

        horizontalLayout->addWidget(closePushButton);


        gridLayout->addLayout(horizontalLayout, 2, 0, 1, 1);

#ifndef QT_NO_SHORTCUT
        nameLabel->setBuddy(nameLineEdit);
        descriptionLabel->setBuddy(descriptionLineEdit);
        dateJoinedLabel->setBuddy(dateJoinedInitDateEdit);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(nameLineEdit, descriptionLineEdit);
        QWidget::setTabOrder(descriptionLineEdit, isActivedCheckBox);
        QWidget::setTabOrder(isActivedCheckBox, dateJoinedInitDateEdit);
        QWidget::setTabOrder(dateJoinedInitDateEdit, dateJoinedEndDateEdit);
        QWidget::setTabOrder(dateJoinedEndDateEdit, searchPushButton);
        QWidget::setTabOrder(searchPushButton, cancelPushButton);

        retranslateUi(NotebookSearch);
        QObject::connect(isActivedLabel, SIGNAL(linkActivated(QString)), isActivedCheckBox, SLOT(toggle()));

        QMetaObject::connectSlotsByName(NotebookSearch);
    } // setupUi

    void retranslateUi(QDialog *NotebookSearch)
    {
        NotebookSearch->setWindowTitle(QApplication::translate("NotebookSearch", "Search advanced", 0, QApplication::UnicodeUTF8));
        titleLabel->setText(QApplication::translate("NotebookSearch", "Search advanced", 0, QApplication::UnicodeUTF8));
        detailsSearchGroupBox->setTitle(QApplication::translate("NotebookSearch", "&Details of Search", 0, QApplication::UnicodeUTF8));
        nameLabel->setText(QApplication::translate("NotebookSearch", "&Name:", 0, QApplication::UnicodeUTF8));
        descriptionLabel->setText(QApplication::translate("NotebookSearch", "&Description:", 0, QApplication::UnicodeUTF8));
        dateJoinedLabel->setText(QApplication::translate("NotebookSearch", "Date &joined:", 0, QApplication::UnicodeUTF8));
        dateJoinedInitDateEdit->setDisplayFormat(QApplication::translate("NotebookSearch", "dd/MM/yyyy", 0, QApplication::UnicodeUTF8));
        toLabel->setText(QApplication::translate("NotebookSearch", "to:", 0, QApplication::UnicodeUTF8));
        dateJoinedEndDateEdit->setDisplayFormat(QApplication::translate("NotebookSearch", "dd/MM/yyyy", 0, QApplication::UnicodeUTF8));
        isActivedLabel->setText(QApplication::translate("NotebookSearch", "Is Actived:", 0, QApplication::UnicodeUTF8));
        isActivedCheckBox->setText(QApplication::translate("NotebookSearch", "Active", 0, QApplication::UnicodeUTF8));
        searchPushButton->setText(QApplication::translate("NotebookSearch", "&Search", 0, QApplication::UnicodeUTF8));
        searchPushButton->setShortcut(QApplication::translate("NotebookSearch", "Ctrl+S", 0, QApplication::UnicodeUTF8));
        cancelPushButton->setText(QApplication::translate("NotebookSearch", "&Cancel", 0, QApplication::UnicodeUTF8));
        cancelPushButton->setShortcut(QApplication::translate("NotebookSearch", "Ctrl+Shift+W", 0, QApplication::UnicodeUTF8));
        closePushButton->setText(QApplication::translate("NotebookSearch", "&Close", 0, QApplication::UnicodeUTF8));
        closePushButton->setShortcut(QApplication::translate("NotebookSearch", "Ctrl+W", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class NotebookSearch: public Ui_NotebookSearch {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_NOTEBOOKSEARCH_H
