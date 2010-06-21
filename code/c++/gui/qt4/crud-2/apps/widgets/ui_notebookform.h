/********************************************************************************
** Form generated from reading UI file 'notebookform.ui'
**
** Created: Thu Jun 17 16:40:51 2010
**      by: Qt User Interface Compiler version 4.6.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_NOTEBOOKFORM_H
#define UI_NOTEBOOKFORM_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QGroupBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTextEdit>

QT_BEGIN_NAMESPACE

class Ui_NotebookForm
{
public:
    QGridLayout *gridLayout_3;
    QHBoxLayout *horizontalLayout_2;
    QLabel *titleLabel;
    QSpacerItem *horizontalSpacer_3;
    QLabel *statusLabel;
    QFrame *frame;
    QGridLayout *gridLayout_2;
    QGroupBox *notebookGroupBox;
    QGridLayout *gridLayout;
    QLabel *nameLabel;
    QLabel *descriptionLabel;
    QTextEdit *descriptionTextEdit;
    QSpacerItem *horizontalSpacer_2;
    QCheckBox *isActivedCheckBox;
    QLineEdit *nameLineEdit;
    QHBoxLayout *horizontalLayout;
    QPushButton *removePushButton;
    QSpacerItem *horizontalSpacer;
    QPushButton *savePushButton;
    QPushButton *saveAndContinueSavingPushButton;
    QPushButton *cancelPushButton;

    void setupUi(QDialog *NotebookForm)
    {
        if (NotebookForm->objectName().isEmpty())
            NotebookForm->setObjectName(QString::fromUtf8("NotebookForm"));
        NotebookForm->resize(623, 448);
        QIcon icon;
        icon.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/face-devilish.png"), QSize(), QIcon::Normal, QIcon::Off);
        NotebookForm->setWindowIcon(icon);
        gridLayout_3 = new QGridLayout(NotebookForm);
        gridLayout_3->setObjectName(QString::fromUtf8("gridLayout_3"));
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        titleLabel = new QLabel(NotebookForm);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setStyleSheet(QString::fromUtf8("color: rgb(85, 85, 127);\n"
"font: 75 14pt \"DejaVu Sans Mono\";"));

        horizontalLayout_2->addWidget(titleLabel);

        horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_3);


        gridLayout_3->addLayout(horizontalLayout_2, 0, 0, 1, 1);

        statusLabel = new QLabel(NotebookForm);
        statusLabel->setObjectName(QString::fromUtf8("statusLabel"));

        gridLayout_3->addWidget(statusLabel, 1, 0, 1, 1);

        frame = new QFrame(NotebookForm);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        gridLayout_2 = new QGridLayout(frame);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        notebookGroupBox = new QGroupBox(frame);
        notebookGroupBox->setObjectName(QString::fromUtf8("notebookGroupBox"));
        gridLayout = new QGridLayout(notebookGroupBox);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        nameLabel = new QLabel(notebookGroupBox);
        nameLabel->setObjectName(QString::fromUtf8("nameLabel"));

        gridLayout->addWidget(nameLabel, 0, 0, 1, 1);

        descriptionLabel = new QLabel(notebookGroupBox);
        descriptionLabel->setObjectName(QString::fromUtf8("descriptionLabel"));

        gridLayout->addWidget(descriptionLabel, 2, 0, 1, 2);

        descriptionTextEdit = new QTextEdit(notebookGroupBox);
        descriptionTextEdit->setObjectName(QString::fromUtf8("descriptionTextEdit"));

        gridLayout->addWidget(descriptionTextEdit, 3, 0, 1, 3);

        horizontalSpacer_2 = new QSpacerItem(407, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer_2, 4, 0, 1, 1);

        isActivedCheckBox = new QCheckBox(notebookGroupBox);
        isActivedCheckBox->setObjectName(QString::fromUtf8("isActivedCheckBox"));
        isActivedCheckBox->setChecked(true);

        gridLayout->addWidget(isActivedCheckBox, 4, 2, 1, 1);

        nameLineEdit = new QLineEdit(notebookGroupBox);
        nameLineEdit->setObjectName(QString::fromUtf8("nameLineEdit"));

        gridLayout->addWidget(nameLineEdit, 1, 0, 1, 1);


        gridLayout_2->addWidget(notebookGroupBox, 0, 0, 1, 1);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        removePushButton = new QPushButton(frame);
        removePushButton->setObjectName(QString::fromUtf8("removePushButton"));
        QIcon icon1;
        icon1.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/edit-delete.png"), QSize(), QIcon::Normal, QIcon::Off);
        removePushButton->setIcon(icon1);

        horizontalLayout->addWidget(removePushButton);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        savePushButton = new QPushButton(frame);
        savePushButton->setObjectName(QString::fromUtf8("savePushButton"));
        QIcon icon2;
        icon2.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-ok-apply.png"), QSize(), QIcon::Normal, QIcon::Off);
        savePushButton->setIcon(icon2);

        horizontalLayout->addWidget(savePushButton);

        saveAndContinueSavingPushButton = new QPushButton(frame);
        saveAndContinueSavingPushButton->setObjectName(QString::fromUtf8("saveAndContinueSavingPushButton"));
        QIcon icon3;
        icon3.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-ok.png"), QSize(), QIcon::Normal, QIcon::Off);
        saveAndContinueSavingPushButton->setIcon(icon3);

        horizontalLayout->addWidget(saveAndContinueSavingPushButton);

        cancelPushButton = new QPushButton(frame);
        cancelPushButton->setObjectName(QString::fromUtf8("cancelPushButton"));
        QIcon icon4;
        icon4.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-cancel.png"), QSize(), QIcon::Normal, QIcon::Off);
        cancelPushButton->setIcon(icon4);

        horizontalLayout->addWidget(cancelPushButton);


        gridLayout_2->addLayout(horizontalLayout, 1, 0, 1, 1);


        gridLayout_3->addWidget(frame, 2, 0, 1, 1);

#ifndef QT_NO_SHORTCUT
        nameLabel->setBuddy(nameLineEdit);
        descriptionLabel->setBuddy(descriptionTextEdit);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(nameLineEdit, descriptionTextEdit);
        QWidget::setTabOrder(descriptionTextEdit, isActivedCheckBox);
        QWidget::setTabOrder(isActivedCheckBox, savePushButton);
        QWidget::setTabOrder(savePushButton, saveAndContinueSavingPushButton);
        QWidget::setTabOrder(saveAndContinueSavingPushButton, removePushButton);
        QWidget::setTabOrder(removePushButton, cancelPushButton);

        retranslateUi(NotebookForm);

        QMetaObject::connectSlotsByName(NotebookForm);
    } // setupUi

    void retranslateUi(QDialog *NotebookForm)
    {
        NotebookForm->setWindowTitle(QApplication::translate("NotebookForm", "Notebook :: Form", 0, QApplication::UnicodeUTF8));
        titleLabel->setText(QApplication::translate("NotebookForm", "Add notebook", 0, QApplication::UnicodeUTF8));
        statusLabel->setText(QString());
        notebookGroupBox->setTitle(QApplication::translate("NotebookForm", "&Notebook details", 0, QApplication::UnicodeUTF8));
        nameLabel->setText(QApplication::translate("NotebookForm", "&Name:", 0, QApplication::UnicodeUTF8));
        descriptionLabel->setText(QApplication::translate("NotebookForm", "&Description:", 0, QApplication::UnicodeUTF8));
        isActivedCheckBox->setText(QApplication::translate("NotebookForm", "Is &Actived", 0, QApplication::UnicodeUTF8));
        removePushButton->setText(QApplication::translate("NotebookForm", "&Remove", 0, QApplication::UnicodeUTF8));
        removePushButton->setShortcut(QApplication::translate("NotebookForm", "Ctrl+Del", 0, QApplication::UnicodeUTF8));
        savePushButton->setText(QApplication::translate("NotebookForm", "&Save", 0, QApplication::UnicodeUTF8));
        savePushButton->setShortcut(QApplication::translate("NotebookForm", "Ctrl+S", 0, QApplication::UnicodeUTF8));
        saveAndContinueSavingPushButton->setText(QApplication::translate("NotebookForm", "Save and &continue saving", 0, QApplication::UnicodeUTF8));
        saveAndContinueSavingPushButton->setShortcut(QApplication::translate("NotebookForm", "Ctrl+Shift+S", 0, QApplication::UnicodeUTF8));
        cancelPushButton->setText(QApplication::translate("NotebookForm", "&Cancel", 0, QApplication::UnicodeUTF8));
        cancelPushButton->setShortcut(QApplication::translate("NotebookForm", "Ctrl+W", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class NotebookForm: public Ui_NotebookForm {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_NOTEBOOKFORM_H
