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
 *    this software without specific prior written group.
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
#include "group.h"

Group::Group(QWidget *parent) :
	QWidget(parent) {
	setupUi(this);
	statusTimer = new QTimer(this);
	createModels();
	createViews();
	createActions();
	updateModels();
	updateWidgets();
}

Group::~Group(void) {
}

void Group::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void Group::newAction(void) {
	GroupForm *form = new GroupForm();
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
	form->raise();
	form->activateWindow();
}

void Group::activateAction(void) {
	qDebug() << "In Group::activateAction()";
	qDebug() << "Not implemented";
}

void Group::desactivateAction(void) {
	qDebug() << "In Group::desactivateAction()";
	qDebug() << "Not implemented";
}

void Group::removeAction(void) {
	QModelIndex index = groupTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = groupModel->record(index.row());
	int id = record.value(group_id).toInt();
	QString name = record.value(group_name).toString();

	QMessageBox msgBox;
	msgBox.setText("Are you sure?");
	msgBox.setInformativeText(QString(qApp->tr(
			"Are you sure you want to delete the selected group objects?\n"
				"All of the following objects and their related items will be "
				"deleted:\n\nGroup: %1\n").arg(name)));
	msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
			| QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::No);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	GroupModel *m = new GroupModel();
	GroupModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted group."));
	updateModels();
}

void Group::searchAdvancedAction(bool checked) {
	if (checked) {
		groupSearch = new GroupSearch(groupModel);
		groupSearch->setAttribute(Qt::WA_DeleteOnClose);
		connect(groupSearch, SIGNAL(formSearched()), this, SLOT(
				updateSearchForm()));
		connect(groupSearch, SIGNAL(formSearchClose()), this, SLOT(
				updateSearchFormClose()));
		//connect(groupSearch, SIGNAL(hide()), this, SLOT(formSearchClose()));
		groupSearch->show();
		groupSearch->raise();
		groupSearch->activateWindow();
	} else {
		groupSearch->close();
	}
}

void Group::searchTextChangedAction(const QString &text) {
	if (text.isEmpty() || text.isNull()) {
		groupModel->setFilter("");
	} else {
		groupModel->setFilter(QString("name LIKE '%1\%'").arg(text));
	}
	updateModels();
}

void Group::doubleClickedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = groupModel->record(index.row());
	int id = record.value(group_id).toInt();

	GroupForm *form = new GroupForm(id);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
	form->raise();
	form->activateWindow();
}

void Group::lastestAction(void) {
	groupModel->query().first();
	updateModels();
}

void Group::nextAction(void) {
	groupModel->query().next();
	updateModels();
}

void Group::previousAction(void) {
	groupModel->query().previous();
	updateModels();
}

void Group::oldestAction(void) {
	groupModel->query().last();
	updateModels();
}

void Group::createModels(void) {
	groupModel = new QSqlRelationalTableModel(this);
	groupModel->setTable("auth_group");
	groupModel->setHeaderData(group_id, Qt::Horizontal, qApp->tr("Id"));
	groupModel->setHeaderData(group_name, Qt::Horizontal, qApp->tr("Name"));
	groupModel->setSort(group_id, Qt::DescendingOrder);
}

void Group::createViews(void) {
	groupTableView->setModel(groupModel);
	groupTableView->setItemDelegate(new QSqlRelationalDelegate(groupTableView));
	groupTableView->setSelectionMode(QAbstractItemView::SingleSelection);
	groupTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	groupTableView->resizeColumnsToContents();
	groupTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);

	QHeaderView *header = groupTableView->horizontalHeader();
	header->setStretchLastSection(true);
}

void Group::createActions(void) {
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

	connect(groupTableView, SIGNAL(doubleClicked(const QModelIndex &)),
			this, SLOT(doubleClickedItemViewAction(const QModelIndex &)));

	connect(latestPushButton, SIGNAL(released()), this, SLOT(lastestAction()));
	connect(nextPushButton, SIGNAL(released()), this, SLOT(nextAction()));
	connect(previousPushButton, SIGNAL(released()), this,
			SLOT(previousAction()));
	connect(oldestPushButton, SIGNAL(released()), this, SLOT(oldestAction()));
}

void Group::updateWidgets(void) {
	if (groupModel->query().size() > 0) {
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

void Group::updateModels(void) {
	groupModel->select();
	qDebug() << "Query:" << groupModel->query().lastQuery();

	int size = groupModel->query().size() < 0 ? 0 : groupModel->query().size();
	statusTableViewLabel->setText(size > 1 ? QString(qApp->tr("%1 group")).arg(
			size) : QString(qApp->tr("%1 group")).arg(size));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(1).arg(25).arg(size));
}

void Group::timerStatus(void) {
	statusTimer->start(3000);
}

void Group::okStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: green; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid green;");
	statusLabel->show();
	timerStatus();
}

void Group::infoStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: blue; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid blue;");
	statusLabel->show();
	timerStatus();
}

void Group::alertStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: yellow; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid yellow;");
	statusLabel->show();
	timerStatus();
}

void Group::errorStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: red; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid red;");
	statusLabel->show();
	timerStatus();
}

void Group::updateSearchForm(void) {
	searchLineEdit->clear();
	updateModels();
}

void Group::updateSearchFormClose(void) {
	searchLineEdit->clear();
	searchAdvancedToolButton->setChecked(false);
	updateModels();
}
