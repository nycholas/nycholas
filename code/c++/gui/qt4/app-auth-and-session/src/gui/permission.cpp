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
	updateWidgets();
}

Permission::~Permission(void) {
}

void Permission::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void Permission::newAction(void) {
	PermissionForm *form = new PermissionForm();
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
	form->raise();
	form->activateWindow();
}

void Permission::activateAction(void) {
	qDebug() << "In Permission::activateAction()";
	qDebug() << "Not implemented";
}

void Permission::desactivateAction(void) {
	qDebug() << "In Permission::desactivateAction()";
	qDebug() << "Not implemented";
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
	msgBox.setDefaultButton(QMessageBox::No);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	PermissionModel *m = new PermissionModel();
	PermissionModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted permission."));
	updateModels();
}

void Permission::searchAdvancedAction(bool checked) {
	if (checked) {
		permissionSearch = new PermissionSearch(permissionModel);
		permissionSearch->setAttribute(Qt::WA_DeleteOnClose);
		connect(permissionSearch, SIGNAL(formSearched()), this, SLOT(
				updateSearchForm()));
		connect(permissionSearch, SIGNAL(formSearchClose()), this, SLOT(
				updateSearchFormClose()));
		//connect(permissionSearch, SIGNAL(hide()), this, SLOT(formSearchClose()));
		permissionSearch->show();
		permissionSearch->raise();
		permissionSearch->activateWindow();
	} else {
		permissionSearch->close();
	}
}

void Permission::searchTextChangedAction(const QString &text) {
	if (text.isEmpty() || text.isNull()) {
		permissionModel->setFilter("");
	} else {
		permissionModel->setFilter(QString("name LIKE '%1\%'").arg(text));
	}
	updateModels();
}

void Permission::doubleClickedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = permissionModel->record(index.row());
	int id = record.value(permission_id).toInt();

	PermissionForm *form = new PermissionForm(id);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
	form->raise();
	form->activateWindow();
}

void Permission::lastestAction(void) {
	permissionModel->query().first();
	updateModels();
}

void Permission::nextAction(void) {
	permissionModel->query().next();
	updateModels();
}

void Permission::previousAction(void) {
	permissionModel->query().previous();
	updateModels();
}

void Permission::oldestAction(void) {
	permissionModel->query().last();
	updateModels();
}

void Permission::createModels(void) {
	permissionModel = new QSqlRelationalTableModel(this);
	permissionModel->setTable("auth_permission");
	permissionModel->setRelation(permission_contentTypes, QSqlRelation(
			"content_types", "app_content_types_id", "name"));
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
}

void Permission::createActions(void) {
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

	connect(permissionTableView, SIGNAL(doubleClicked(const QModelIndex &)),
			this, SLOT(doubleClickedItemViewAction(const QModelIndex &)));

	connect(latestPushButton, SIGNAL(released()), this, SLOT(lastestAction()));
	connect(nextPushButton, SIGNAL(released()), this, SLOT(nextAction()));
	connect(previousPushButton, SIGNAL(released()), this,
			SLOT(previousAction()));
	connect(oldestPushButton, SIGNAL(released()), this, SLOT(oldestAction()));
}

void Permission::updateWidgets(void) {
	if (permissionModel->query().size() > 0) {
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

void Permission::updateModels(void) {
	permissionModel->select();
	qDebug() << "Query:" << permissionModel->query().lastQuery();

	int size = permissionModel->query().size() < 0 ? 0
			: permissionModel->query().size();
	statusTableViewLabel->setText(
			size > 1 ? QString(qApp->tr("%1 permission")).arg(size) : QString(
					qApp->tr("%1 permission")).arg(size));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(1).arg(25).arg(size));
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
	searchAdvancedToolButton->setChecked(false);
	updateModels();
}
