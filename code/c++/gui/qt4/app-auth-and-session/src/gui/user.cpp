/**
 * Simple example Qt - CRUD.
 * Copyright (c) 2010, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *  * Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
 *    its contributors may be used to endorse or promote products derived from
 *    this software without specific prior written user.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include "user.h"

User::User(QWidget *parent) :
	QWidget(parent) {
	setupUi(this);
	statusTimer = new QTimer(this);
	createModels();
	createViews();
	createActions();
	updateModels();
	updateWidgets();
}

User::~User(void) {
}

void User::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void User::newAction(void) {
	UserForm *form = new UserForm();
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
	form->raise();
	form->activateWindow();
}

void User::activateAction(void) {
	qDebug() << "In User::activateAction()";
	qDebug() << "Not implemented";
}

void User::desactivateAction(void) {
	qDebug() << "In User::desactivateAction()";
	qDebug() << "Not implemented";
}

void User::removeAction(void) {
	QModelIndex index = userTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = userModel->record(index.row());
	int id = record.value(user_id).toInt();
	QString name = record.value(user_name).toString();

	QMessageBox msgBox;
	msgBox.setText("Are you sure?");
	msgBox.setInformativeText(QString(qApp->tr(
			"Are you sure you want to delete the selected user objects?\n"
				"All of the following objects and their related items will be "
				"deleted:\n\nUser: %1\n").arg(name)));
	msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
			| QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::No);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	UserModel *m = new UserModel();
	UserModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted user."));
	updateModels();
}

void User::searchAdvancedAction(bool checked) {
	if (checked) {
		userSearch = new UserSearch(userModel);
		userSearch->setAttribute(Qt::WA_DeleteOnClose);
		connect(userSearch, SIGNAL(formSearched()), this, SLOT(
				updateSearchForm()));
		connect(userSearch, SIGNAL(formSearchClose()), this, SLOT(
				updateSearchFormClose()));
		//connect(userSearch, SIGNAL(hide()), this, SLOT(formSearchClose()));
		userSearch->show();
		userSearch->raise();
		userSearch->activateWindow();
	} else {
		userSearch->close();
	}
}

void User::searchTextChangedAction(const QString &text) {
	if (text.isEmpty() || text.isNull()) {
		userModel->setFilter("");
	} else {
		userModel->setFilter(QString("name LIKE '%1\%'").arg(text));
	}
	updateModels();
}

void User::doubleClickedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = userModel->record(index.row());
	int id = record.value(user_id).toInt();

	UserForm *form = new UserForm(id);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
	form->raise();
	form->activateWindow();
}

void User::lastestAction(void) {
	userModel->query().first();
	updateModels();
}

void User::nextAction(void) {
	userModel->query().next();
	updateModels();
}

void User::previousAction(void) {
	userModel->query().previous();
	updateModels();
}

void User::oldestAction(void) {
	userModel->query().last();
	updateModels();
}

void User::createModels(void) {
	userModel = new QSqlRelationalTableModel(this);
	userModel->setTable("auth_user");
	userModel->setHeaderData(user_id, Qt::Horizontal, qApp->tr("Id"));
	userModel->setHeaderData(user_name, Qt::Horizontal, qApp->tr("Name"));
	userModel->setSort(user_id, Qt::DescendingOrder);
}

void User::createViews(void) {
	userTableView->setModel(userModel);
	userTableView->setItemDelegate(new QSqlRelationalDelegate(userTableView));
	userTableView->setSelectionMode(QAbstractItemView::SingleSelection);
	userTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	userTableView->resizeColumnsToContents();
	userTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);

	QHeaderView *header = userTableView->horizontalHeader();
	header->setStretchLastSection(true);
}

void User::createActions(void) {
	connect(statusTimer, SIGNAL(timeout()), this, SLOT(timerStatusAction()));

	connect(newPushButton, SIGNAL(released()), this, SLOT(newAction()));
	connect(activatePushButton, SIGNAL(released()), this,
			SLOT(activateAction()));
	connect(deactivatePushButton, SIGNAL(released()), this, SLOT(
			desactivateAction()));
	connect(removePushButton, SIGNAL(released()), this, SLOT(removeAction()));
	connect(searchAdvancedToolButton, SIGNAL(toggled(bool)), this,
			SLOT(searchAdvancedAction(bool)));
	connect(searchLineEdit, SIGNAL(textChanged(const QString &)), this,
			SLOT(searchTextChangedAction(const QString &)));

	connect(userTableView, SIGNAL(doubleClicked(const QModelIndex &)), this,
			SLOT(doubleClickedItemViewAction(const QModelIndex &)));

	connect(latestPushButton, SIGNAL(released()), this, SLOT(lastestAction()));
	connect(nextPushButton, SIGNAL(released()), this, SLOT(nextAction()));
	connect(previousPushButton, SIGNAL(released()), this,
			SLOT(previousAction()));
	connect(oldestPushButton, SIGNAL(released()), this, SLOT(oldestAction()));
}

void User::updateWidgets(void) {
	if (userModel->query().size() > 0) {
		//activatePushButton->show();
		//deactivatePushButton->show();
		removePushButton->show();
	} else {
		//activatePushButton->hide();
		//deactivatePushButton->hide();
		removePushButton->hide();
	}
	activatePushButton->hide();
	deactivatePushButton->hide();
	statusLabel->hide();
}

void User::updateModels(void) {
	userModel->select();
	qDebug() << "Query:" << userModel->query().lastQuery();

	int size = userModel->query().size() < 0 ? 0 : userModel->query().size();
	statusTableViewLabel->setText(size > 1 ? QString(qApp->tr("%1 user")).arg(
			size) : QString(qApp->tr("%1 user")).arg(size));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(1).arg(25).arg(size));
}

void User::timerStatus(void) {
	statusTimer->start(3000);
}

void User::okStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: green; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid green;");
	statusLabel->show();
	timerStatus();
}

void User::infoStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: blue; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid blue;");
	statusLabel->show();
	timerStatus();
}

void User::alertStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: yellow; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid yellow;");
	statusLabel->show();
	timerStatus();
}

void User::errorStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: red; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid red;");
	statusLabel->show();
	timerStatus();
}

void User::updateSearchForm(void) {
	searchLineEdit->clear();
	updateModels();
}

void User::updateSearchFormClose(void) {
	searchLineEdit->clear();
	searchAdvancedToolButton->setChecked(false);
	updateModels();
}
