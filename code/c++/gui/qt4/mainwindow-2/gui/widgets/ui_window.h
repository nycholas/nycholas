/********************************************************************************
** Form generated from reading UI file 'window.ui'
**
** Created: Fri Jun 4 23:17:26 2010
**      by: Qt User Interface Compiler version 4.6.2
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_WINDOW_H
#define UI_WINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QGroupBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QListWidget>
#include <QtGui/QPushButton>
#include <QtGui/QRadioButton>
#include <QtGui/QScrollBar>
#include <QtGui/QSlider>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Window
{
public:
    QGridLayout *gridLayout_2;
    QGroupBox *groupBox;
    QGridLayout *gridLayout;
    QLabel *label;
    QCheckBox *checkBox;
    QCheckBox *checkBox_2;
    QRadioButton *radioButton;
    QRadioButton *radioButton_2;
    QPushButton *pushButton;
    QToolButton *toolButton;
    QToolButton *toolButton_2;
    QPushButton *pushButton_2;
    QScrollBar *horizontalScrollBar;
    QFrame *line;
    QListWidget *listWidget;
    QSlider *horizontalSlider;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QPushButton *buttonAbout;
    QPushButton *buttonApply;
    QPushButton *buttonOk;
    QPushButton *buttonCancel;

    void setupUi(QWidget *Window)
    {
        if (Window->objectName().isEmpty())
            Window->setObjectName(QString::fromUtf8("Window"));
        Window->resize(452, 384);
        QIcon icon;
        icon.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/face-devilish.png"), QSize(), QIcon::Normal, QIcon::Off);
        Window->setWindowIcon(icon);
        gridLayout_2 = new QGridLayout(Window);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        groupBox = new QGroupBox(Window);
        groupBox->setObjectName(QString::fromUtf8("groupBox"));
        gridLayout = new QGridLayout(groupBox);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        label = new QLabel(groupBox);
        label->setObjectName(QString::fromUtf8("label"));

        gridLayout->addWidget(label, 0, 0, 1, 1);

        checkBox = new QCheckBox(groupBox);
        checkBox->setObjectName(QString::fromUtf8("checkBox"));

        gridLayout->addWidget(checkBox, 1, 0, 1, 1);

        checkBox_2 = new QCheckBox(groupBox);
        checkBox_2->setObjectName(QString::fromUtf8("checkBox_2"));
        checkBox_2->setChecked(true);

        gridLayout->addWidget(checkBox_2, 1, 1, 1, 2);

        radioButton = new QRadioButton(groupBox);
        radioButton->setObjectName(QString::fromUtf8("radioButton"));
        radioButton->setChecked(true);

        gridLayout->addWidget(radioButton, 1, 3, 1, 2);

        radioButton_2 = new QRadioButton(groupBox);
        radioButton_2->setObjectName(QString::fromUtf8("radioButton_2"));

        gridLayout->addWidget(radioButton_2, 1, 5, 1, 1);

        pushButton = new QPushButton(groupBox);
        pushButton->setObjectName(QString::fromUtf8("pushButton"));
        QIcon icon1;
        icon1.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/tools-report-bug.png"), QSize(), QIcon::Normal, QIcon::Off);
        pushButton->setIcon(icon1);

        gridLayout->addWidget(pushButton, 2, 0, 1, 1);

        toolButton = new QToolButton(groupBox);
        toolButton->setObjectName(QString::fromUtf8("toolButton"));
        toolButton->setCheckable(true);

        gridLayout->addWidget(toolButton, 2, 1, 1, 1);

        toolButton_2 = new QToolButton(groupBox);
        toolButton_2->setObjectName(QString::fromUtf8("toolButton_2"));
        toolButton_2->setCheckable(true);
        toolButton_2->setChecked(true);

        gridLayout->addWidget(toolButton_2, 2, 2, 1, 2);

        pushButton_2 = new QPushButton(groupBox);
        pushButton_2->setObjectName(QString::fromUtf8("pushButton_2"));
        pushButton_2->setEnabled(false);

        gridLayout->addWidget(pushButton_2, 2, 5, 1, 1);

        horizontalScrollBar = new QScrollBar(groupBox);
        horizontalScrollBar->setObjectName(QString::fromUtf8("horizontalScrollBar"));
        horizontalScrollBar->setOrientation(Qt::Horizontal);

        gridLayout->addWidget(horizontalScrollBar, 3, 3, 1, 3);

        line = new QFrame(groupBox);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(line, 4, 1, 1, 3);

        listWidget = new QListWidget(groupBox);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        new QListWidgetItem(listWidget);
        listWidget->setObjectName(QString::fromUtf8("listWidget"));

        gridLayout->addWidget(listWidget, 5, 0, 1, 6);

        horizontalSlider = new QSlider(groupBox);
        horizontalSlider->setObjectName(QString::fromUtf8("horizontalSlider"));
        horizontalSlider->setOrientation(Qt::Horizontal);

        gridLayout->addWidget(horizontalSlider, 3, 0, 1, 2);


        gridLayout_2->addWidget(groupBox, 0, 0, 1, 1);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setSizeConstraint(QLayout::SetFixedSize);
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        buttonAbout = new QPushButton(Window);
        buttonAbout->setObjectName(QString::fromUtf8("buttonAbout"));
        buttonAbout->setMaximumSize(QSize(16777215, 23));
        QIcon icon2;
        icon2.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-information.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonAbout->setIcon(icon2);

        horizontalLayout->addWidget(buttonAbout);

        buttonApply = new QPushButton(Window);
        buttonApply->setObjectName(QString::fromUtf8("buttonApply"));
        buttonApply->setMaximumSize(QSize(16777215, 23));
        QIcon icon3;
        icon3.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-ok-apply.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonApply->setIcon(icon3);

        horizontalLayout->addWidget(buttonApply);

        buttonOk = new QPushButton(Window);
        buttonOk->setObjectName(QString::fromUtf8("buttonOk"));
        buttonOk->setMaximumSize(QSize(16777215, 23));
        QIcon icon4;
        icon4.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-ok.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonOk->setIcon(icon4);

        horizontalLayout->addWidget(buttonOk);

        buttonCancel = new QPushButton(Window);
        buttonCancel->setObjectName(QString::fromUtf8("buttonCancel"));
        buttonCancel->setMaximumSize(QSize(16777215, 23));
        QIcon icon5;
        icon5.addFile(QString::fromUtf8(":/default/static/default/icons/22x22/dialog-cancel.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonCancel->setIcon(icon5);

        horizontalLayout->addWidget(buttonCancel);


        gridLayout_2->addLayout(horizontalLayout, 1, 0, 1, 1);


        retranslateUi(Window);

        QMetaObject::connectSlotsByName(Window);
    } // setupUi

    void retranslateUi(QWidget *Window)
    {
        Window->setWindowTitle(QApplication::translate("Window", "Window", 0, QApplication::UnicodeUTF8));
        groupBox->setTitle(QApplication::translate("Window", "Preview", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("Window", "Menu", 0, QApplication::UnicodeUTF8));
        checkBox->setText(QApplication::translate("Window", "CheckBox", 0, QApplication::UnicodeUTF8));
        checkBox_2->setText(QApplication::translate("Window", "CheckBox", 0, QApplication::UnicodeUTF8));
        radioButton->setText(QApplication::translate("Window", "RadioButton", 0, QApplication::UnicodeUTF8));
        radioButton_2->setText(QApplication::translate("Window", "RadioButton", 0, QApplication::UnicodeUTF8));
        pushButton->setText(QApplication::translate("Window", "Button", 0, QApplication::UnicodeUTF8));
        toolButton->setText(QApplication::translate("Window", "Toogle", 0, QApplication::UnicodeUTF8));
        toolButton_2->setText(QApplication::translate("Window", "Toogle", 0, QApplication::UnicodeUTF8));
        pushButton_2->setText(QApplication::translate("Window", "Disabled", 0, QApplication::UnicodeUTF8));

        const bool __sortingEnabled = listWidget->isSortingEnabled();
        listWidget->setSortingEnabled(false);
        QListWidgetItem *___qlistwidgetitem = listWidget->item(0);
        ___qlistwidgetitem->setText(QApplication::translate("Window", "Aerosmith", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem1 = listWidget->item(1);
        ___qlistwidgetitem1->setText(QApplication::translate("Window", "AC/DC", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem2 = listWidget->item(2);
        ___qlistwidgetitem2->setText(QApplication::translate("Window", "After Forever", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem3 = listWidget->item(3);
        ___qlistwidgetitem3->setText(QApplication::translate("Window", "Airboune", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem4 = listWidget->item(4);
        ___qlistwidgetitem4->setText(QApplication::translate("Window", "Alice In Chains", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem5 = listWidget->item(5);
        ___qlistwidgetitem5->setText(QApplication::translate("Window", "The Allman Brothers Band", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem6 = listWidget->item(6);
        ___qlistwidgetitem6->setText(QApplication::translate("Window", "Angra", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem7 = listWidget->item(7);
        ___qlistwidgetitem7->setText(QApplication::translate("Window", "Audioslave", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem8 = listWidget->item(8);
        ___qlistwidgetitem8->setText(QApplication::translate("Window", "Avantasia", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem9 = listWidget->item(9);
        ___qlistwidgetitem9->setText(QApplication::translate("Window", "Baltazares", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem10 = listWidget->item(10);
        ___qlistwidgetitem10->setText(QApplication::translate("Window", "Baranga", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem11 = listWidget->item(11);
        ___qlistwidgetitem11->setText(QApplication::translate("Window", "The Black Crowes", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem12 = listWidget->item(12);
        ___qlistwidgetitem12->setText(QApplication::translate("Window", "Black Label Society", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem13 = listWidget->item(13);
        ___qlistwidgetitem13->setText(QApplication::translate("Window", "Black Sabbath", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem14 = listWidget->item(14);
        ___qlistwidgetitem14->setText(QApplication::translate("Window", "Blitzkid", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem15 = listWidget->item(15);
        ___qlistwidgetitem15->setText(QApplication::translate("Window", "Brasileiros S/A", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem16 = listWidget->item(16);
        ___qlistwidgetitem16->setText(QApplication::translate("Window", "Bruce Dickinson", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem17 = listWidget->item(17);
        ___qlistwidgetitem17->setText(QApplication::translate("Window", "Cachorro Grande", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem18 = listWidget->item(18);
        ___qlistwidgetitem18->setText(QApplication::translate("Window", "Carey, Bell, Billy Branch, James Cotton", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem19 = listWidget->item(19);
        ___qlistwidgetitem19->setText(QApplication::translate("Window", "Celso Blues Boy", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem20 = listWidget->item(20);
        ___qlistwidgetitem20->setText(QApplication::translate("Window", "Chicago Blues Harmonica Project", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem21 = listWidget->item(21);
        ___qlistwidgetitem21->setText(QApplication::translate("Window", "Chimaira", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem22 = listWidget->item(22);
        ___qlistwidgetitem22->setText(QApplication::translate("Window", "Chris Cornell", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem23 = listWidget->item(23);
        ___qlistwidgetitem23->setText(QApplication::translate("Window", "Cry of Love", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem24 = listWidget->item(24);
        ___qlistwidgetitem24->setText(QApplication::translate("Window", "Death", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem25 = listWidget->item(25);
        ___qlistwidgetitem25->setText(QApplication::translate("Window", "Deep Purple", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem26 = listWidget->item(26);
        ___qlistwidgetitem26->setText(QApplication::translate("Window", "Dio", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem27 = listWidget->item(27);
        ___qlistwidgetitem27->setText(QApplication::translate("Window", "Dirty Sweet", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem28 = listWidget->item(28);
        ___qlistwidgetitem28->setText(QApplication::translate("Window", "Disturbed", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem29 = listWidget->item(29);
        ___qlistwidgetitem29->setText(QApplication::translate("Window", "Dog Eat Dog", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem30 = listWidget->item(30);
        ___qlistwidgetitem30->setText(QApplication::translate("Window", "The Doors", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem31 = listWidget->item(31);
        ___qlistwidgetitem31->setText(QApplication::translate("Window", "Dream Theater", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem32 = listWidget->item(32);
        ___qlistwidgetitem32->setText(QApplication::translate("Window", "Driver By Truckers", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem33 = listWidget->item(33);
        ___qlistwidgetitem33->setText(QApplication::translate("Window", "Edu Ardanuy", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem34 = listWidget->item(34);
        ___qlistwidgetitem34->setText(QApplication::translate("Window", "Foo Fighters", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem35 = listWidget->item(35);
        ___qlistwidgetitem35->setText(QApplication::translate("Window", "Garotos Podres", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem36 = listWidget->item(36);
        ___qlistwidgetitem36->setText(QApplication::translate("Window", "Glenn Hughes", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem37 = listWidget->item(37);
        ___qlistwidgetitem37->setText(QApplication::translate("Window", "Gnarls Barkley", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem38 = listWidget->item(38);
        ___qlistwidgetitem38->setText(QApplication::translate("Window", "Godsmack", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem39 = listWidget->item(39);
        ___qlistwidgetitem39->setText(QApplication::translate("Window", "Gramofocas", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem40 = listWidget->item(40);
        ___qlistwidgetitem40->setText(QApplication::translate("Window", "Heaven and Hell", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem41 = listWidget->item(41);
        ___qlistwidgetitem41->setText(QApplication::translate("Window", "Iced Earth", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem42 = listWidget->item(42);
        ___qlistwidgetitem42->setText(QApplication::translate("Window", "Incubus", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem43 = listWidget->item(43);
        ___qlistwidgetitem43->setText(QApplication::translate("Window", "Iron Maiden", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem44 = listWidget->item(44);
        ___qlistwidgetitem44->setText(QApplication::translate("Window", "Jaded Sun", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem45 = listWidget->item(45);
        ___qlistwidgetitem45->setText(QApplication::translate("Window", "James Cotton & Charlie Haden", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem46 = listWidget->item(46);
        ___qlistwidgetitem46->setText(QApplication::translate("Window", "Jim Morrison", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem47 = listWidget->item(47);
        ___qlistwidgetitem47->setText(QApplication::translate("Window", "Jimi Hendrix", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem48 = listWidget->item(48);
        ___qlistwidgetitem48->setText(QApplication::translate("Window", "The Johnny O. Band", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem49 = listWidget->item(49);
        ___qlistwidgetitem49->setText(QApplication::translate("Window", "Judas Priest", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem50 = listWidget->item(50);
        ___qlistwidgetitem50->setText(QApplication::translate("Window", "Kiss", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem51 = listWidget->item(51);
        ___qlistwidgetitem51->setText(QApplication::translate("Window", "Led Zeppelin", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem52 = listWidget->item(52);
        ___qlistwidgetitem52->setText(QApplication::translate("Window", "Living Colour", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem53 = listWidget->item(53);
        ___qlistwidgetitem53->setText(QApplication::translate("Window", "Lynyrd Skynyrd", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem54 = listWidget->item(54);
        ___qlistwidgetitem54->setText(QApplication::translate("Window", "Matanza", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem55 = listWidget->item(55);
        ___qlistwidgetitem55->setText(QApplication::translate("Window", "Megadeath", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem56 = listWidget->item(56);
        ___qlistwidgetitem56->setText(QApplication::translate("Window", "Metallica", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem57 = listWidget->item(57);
        ___qlistwidgetitem57->setText(QApplication::translate("Window", "Misfits", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem58 = listWidget->item(58);
        ___qlistwidgetitem58->setText(QApplication::translate("Window", "Motorocker", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem59 = listWidget->item(59);
        ___qlistwidgetitem59->setText(QApplication::translate("Window", "Mot\303\266rhead", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem60 = listWidget->item(60);
        ___qlistwidgetitem60->setText(QApplication::translate("Window", "Murderdolls", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem61 = listWidget->item(61);
        ___qlistwidgetitem61->setText(QApplication::translate("Window", "Nickelback", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem62 = listWidget->item(62);
        ___qlistwidgetitem62->setText(QApplication::translate("Window", "Northern Kings", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem63 = listWidget->item(63);
        ___qlistwidgetitem63->setText(QApplication::translate("Window", "O Rappa", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem64 = listWidget->item(64);
        ___qlistwidgetitem64->setText(QApplication::translate("Window", "Ozzy Osbourne", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem65 = listWidget->item(65);
        ___qlistwidgetitem65->setText(QApplication::translate("Window", "Pantera", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem66 = listWidget->item(66);
        ___qlistwidgetitem66->setText(QApplication::translate("Window", "The Parlor Mob", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem67 = listWidget->item(67);
        ___qlistwidgetitem67->setText(QApplication::translate("Window", "Planet Hemp", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem68 = listWidget->item(68);
        ___qlistwidgetitem68->setText(QApplication::translate("Window", "Radiohead", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem69 = listWidget->item(69);
        ___qlistwidgetitem69->setText(QApplication::translate("Window", "Ratos de Por\303\243o", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem70 = listWidget->item(70);
        ___qlistwidgetitem70->setText(QApplication::translate("Window", "Red Hot Chili Peppers", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem71 = listWidget->item(71);
        ___qlistwidgetitem71->setText(QApplication::translate("Window", "Rock Rocket", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem72 = listWidget->item(72);
        ___qlistwidgetitem72->setText(QApplication::translate("Window", "Scott Weiland", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem73 = listWidget->item(73);
        ___qlistwidgetitem73->setText(QApplication::translate("Window", "Seal", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem74 = listWidget->item(74);
        ___qlistwidgetitem74->setText(QApplication::translate("Window", "Shaman", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem75 = listWidget->item(75);
        ___qlistwidgetitem75->setText(QApplication::translate("Window", "Sonic Youth", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem76 = listWidget->item(76);
        ___qlistwidgetitem76->setText(QApplication::translate("Window", "Soundgarden", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem77 = listWidget->item(77);
        ___qlistwidgetitem77->setText(QApplication::translate("Window", "Steel Dragon", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem78 = listWidget->item(78);
        ___qlistwidgetitem78->setText(QApplication::translate("Window", "Stevie Ray Vaughan", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem79 = listWidget->item(79);
        ___qlistwidgetitem79->setText(QApplication::translate("Window", "Stone Temple Pilots", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem80 = listWidget->item(80);
        ___qlistwidgetitem80->setText(QApplication::translate("Window", "Temple Of The Dog", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem81 = listWidget->item(81);
        ___qlistwidgetitem81->setText(QApplication::translate("Window", "Van Halen", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem82 = listWidget->item(82);
        ___qlistwidgetitem82->setText(QApplication::translate("Window", "Velhas V\303\255rgens", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem83 = listWidget->item(83);
        ___qlistwidgetitem83->setText(QApplication::translate("Window", "Wolfmother", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem84 = listWidget->item(84);
        ___qlistwidgetitem84->setText(QApplication::translate("Window", "Zakk Wylde", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem85 = listWidget->item(85);
        ___qlistwidgetitem85->setText(QApplication::translate("Window", "Zumbis do Espa\303\247o", 0, QApplication::UnicodeUTF8));
        QListWidgetItem *___qlistwidgetitem86 = listWidget->item(86);
        ___qlistwidgetitem86->setText(QApplication::translate("Window", "ZZ Top", 0, QApplication::UnicodeUTF8));
        listWidget->setSortingEnabled(__sortingEnabled);

        buttonAbout->setText(QApplication::translate("Window", "About", 0, QApplication::UnicodeUTF8));
        buttonApply->setText(QApplication::translate("Window", "&Apply", 0, QApplication::UnicodeUTF8));
        buttonOk->setText(QApplication::translate("Window", "&OK", 0, QApplication::UnicodeUTF8));
        buttonCancel->setText(QApplication::translate("Window", "&Cancel", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class Window: public Ui_Window {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_WINDOW_H
