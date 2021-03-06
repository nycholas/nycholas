/**
 * Simple example Qt - Mainwindow with authentication and session.
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
 *    this software without specific prior written permission.
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
#include "permission.h"

Permission::Permission(QWidget *parent) :
	QWidget(parent) {
	setupUi(this);
	statusTimer = new QTimer(this);
	createModels();
	createViews();
	createActions();
	updateModels();
	createSearchAdvanceWidget();
	updateWidgets();
}

Permission::~Permission(void) {
}

void Permission::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void Permission::newAction(void) {
	permissionModel->setId(0);
	PermissionForm *form = new PermissionForm(permissionModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
}

void Permission::activateAction(void) {

}

void Permission::desactivateAction(void) {

}

void Permission::removeAction(void) {
	QModelIndex index = permissionTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = permissionModel->record(index.row());
	int id = record.value(permission_id).toInt();
	QString name = record.value(permission_name).toString();

	QMessageBox msgBox;
	msgBox.setText("Are you sure?");
	msgBox.setInformativeText(QString(qApp->tr(
			"Are you sure you want to delete the selected permission objects?\n"
				"All of the following objects and their related items will be "
				"deleted:\n\nPermission: %1\n").arg(name)));
	msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
			| QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::Yes);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	PermissionModel *m = new PermissionModel(0, this);
	PermissionModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted permission."));
	updateModels();
}

void Permission::createSearchAdvanceWidget(void) {
	permissionSearch = new PermissionSearch(permissionModel);
	connect(permissionSearch, SIGNAL(formSearched()), this, SLOT(
			updateSearchForm()));
	connect(permissionSearch, SIGNAL(formSearchClose()), this, SLOT(
			updateSearchFormClose()));
}

void Permission::searchAdvancedAction(void) {
	if (!permissionSearch->isVisible()) {
		permissionSearch->show();
		permissionSearch->raise();
		permissionSearch->activateWindow();
	} else {
		permissionSearch->hide();
	}
}

void Permission::searchTextChangedAction() {
	QString text = searchLineEdit->text();
	if (text.isEmpty() || text.isNull()) {
		permissionModel->setF("");
	} else {
		permissionModel->setF(QString("name LIKE '%1\%'").arg(text));
	}
	permissionModel->setBegin(0);
	updateModels();
}

void Permission::closeAction(void) {
	close();
}

void Permission::selectedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = permissionModel->record(index.row());
	int id = record.value(permission_id).toInt();
	permissionModel->setId(id);

	PermissionForm *form = new PermissionForm(permissionModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
}

void Permission::lastestAction(void) {
	permissionModel->setBegin(0);
	updateModels();
}

void Permission::nextAction(void) {
	permissionModel->setBegin(permissionModel->begin()
			- permissionModel->limit());
	updateModels();
}

void Permission::previousAction(void) {
	permissionModel->setBegin(permissionModel->begin()
			+ permissionModel->limit());
	updateModels();
}

void Permission::oldestAction(void) {
	int count = permissionModel->count();
	int limit = permissionModel->limit();
	permissionModel->setBegin((limit * count / limit) - limit);
	updateModels();
}

void Permission::createModels(void) {
	permissionModel = new PermissionModel(0, this);
	permissionModel->setTable("auth_permission");
	permissionModel->setHeaderData(permission_id, Qt::Horizontal,
			qApp->tr("Id"));
	permissionModel->setHeaderData(permission_contentTypes, Qt::Horizontal,
			qApp->tr("Content Types"));
	permissionModel->setHeaderData(permission_name, Qt::Horizontal, qApp->tr(
			"Name"));
	permissionModel->setHeaderData(permission_codename, Qt::Horizontal,
			qApp->tr("Codename"));
	permissionModel->setSort(permission_id, Qt::DescendingOrder);
}

void Permission::createViews(void) {
	permissionTableView->setModel(permissionModel);
	permissionTableView->setItemDelegate(new QSqlRelationalDelegate(
			permissionTableView));
	permissionTableView->setSelectionMode(QAbstractItemView::SingleSelection);
	permissionTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	permissionTableView->resizeColumnsToContents();
	permissionTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);

	QHeaderView *header = permissionTableView->horizontalHeader();
	header->setStretchLastSection(true);
connect(header, SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)), this,
		SLOT(updateModels()));
}

void Permission::createActions(void) {
	connect(statusTimer, SIGNAL(timeout()), this, SLOT(timerStatusAction()));

	connect(newPushButton, SIGNAL(released()), this, SLOT(newAction()));
	connect(activatePushButton, SIGNAL(released()), this,
			SLOT(activateAction()));
	connect(deactivatePushButton, SIGNAL(released()), this, SLOT(
			desactivateAction()));
	connect(removePushButton, SIGNAL(released()), this, SLOT(removeAction()));
	connect(searchAdvancedPushButton, SIGNAL(released()), this, SLOT(
			searchAdvancedAction()));
	connect(searchLineEdit, SIGNAL(returnPressed()), this, SLOT(
			searchTextChangedAction()));
	//connect(closePushButton, SIGNAL(released()), this, SLOT(closeAction()));

	connect(permissionTableView, SIGNAL(activated(const QModelIndex &)), this,
			SLOT(selectedItemViewAction(const QModelIndex &)));

	connect(latestPushButton, SIGNAL(released()), this, SLOT(lastestAction()));
	connect(nextPushButton, SIGNAL(released()), this, SLOT(nextAction()));
	connect(previousPushButton, SIGNAL(released()), this,
			SLOT(previousAction()));
	connect(oldestPushButton, SIGNAL(released()), this, SLOT(oldestAction()));
}

void Permission::updateWidgets(void) {
	if (permissionModel->query().size() > 0) {
		activatePushButton->hide();
		deactivatePushButton->hide();
		removePushButton->show();
	} else {
		activatePushButton->hide();
		deactivatePushButton->hide();
		removePushButton->hide();
	}
	permissionTableView->setTabKeyNavigation(true);
	statusLabel->hide();
}

void Permission::updateModels(void) {
	permissionModel->paginator();
	qDebug() << "Query:" << permissionModel->query().lastQuery();

	int begin = permissionModel->begin();
	int limit = permissionModel->limit();
	int sizeAll = permissionModel->count();
	int size = permissionModel->query().size();

	statusPermissionTableViewLabel->setText(sizeAll > 1 ? QString(qApp->tr(
			"%1 permissions")).arg(sizeAll)
			: QString(qApp->tr("%1 permission")).arg(sizeAll));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(begin).arg(limit).arg(
			sizeAll));

	latestPushButton->setEnabled(begin > 0);
	nextPushButton->setEnabled(begin > 0);
	previousPushButton->setEnabled(sizeAll > (size + begin));
	oldestPushButton->setEnabled(sizeAll > (size + begin));
}

void Permission::timerStatus(void) {
	statusTimer->start(3000);
}

void Permission::okStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: green; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid green;");
	statusLabel->show();
	timerStatus();
}

void Permission::infoStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: blue; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid blue;");
	statusLabel->show();
	timerStatus();
}

void Permission::alertStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: yellow; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid yellow;");
	statusLabel->show();
	timerStatus();
}

void Permission::errorStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: red; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid red;");
	statusLabel->show();
	timerStatus();
}

void Permission::updateSearchForm(void) {
	searchLineEdit->clear();
	updateModels();
}

void Permission::updateSearchFormClose(void) {
	searchLineEdit->clear();
	updateModels();
}

void Permission::updateStatus(const QString &msg, int code) {
	switch (code) {
	case 0:
		okStatus(msg);
		break;
	case 1:
		infoStatus(msg);
		break;
	case 2:
		alertStatus(msg);
		break;
	case 3:
		errorStatus(msg);
		break;
	default:
		okStatus(msg);
		break;
	}
}
