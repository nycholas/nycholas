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
	createWidgets();
	updateWidgets();
}

Group::~Group(void) {
}

void Group::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void Group::newAction(void) {
	groupModel->setId(0);
	GroupForm *form = new GroupForm(groupModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
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
	msgBox.setDefaultButton(QMessageBox::Yes);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	GroupModel *m = new GroupModel(0, this);
	GroupModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted group."));
	updateModels();
}

void Group::createWidgets(void) {
	groupSearch = new GroupSearch(groupModel);
	connect(groupSearch, SIGNAL(formSearched()), this, SLOT(updateSearchForm()));
	connect(groupSearch, SIGNAL(formSearchClose()), this, SLOT(
			updateSearchFormClose()));
}

void Group::searchAdvancedAction(bool checked) {
	if (!groupSearch->isVisible()) {
		groupSearch->show();
		groupSearch->raise();
		groupSearch->activateWindow();
	} else {
		groupSearch->hide();
	}
}

void Group::searchTextChangedAction(const QString &text) {
	if (text.isEmpty() || text.isNull()) {
		groupModel->setF("");
	} else {
		groupModel->setF(QString("name LIKE '%1\%'").arg(text));
	}
	groupModel->setBegin(0);
	updateModels();
}

void Group::selectedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = groupModel->record(index.row());
	int id = record.value(group_id).toInt();

	GroupForm *form = new GroupForm(groupModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
}

void Group::lastestAction(void) {
	groupModel->setBegin(0);
	updateModels();
}

void Group::nextAction(void) {
	groupModel->setBegin(groupModel->begin() - groupModel->limit());
	updateModels();
}

void Group::previousAction(void) {
	groupModel->setBegin(groupModel->begin() + groupModel->limit());
	updateModels();
}

void Group::oldestAction(void) {
	int count = groupModel->count();
	int limit = groupModel->limit();
	groupModel->setBegin((limit * count / limit) - limit);
	updateModels();
}

void Group::createModels(void) {
	groupModel = new GroupModel(0, this);
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
connect(header, SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)), this,
		SLOT(updateModels()));
}

void Group::createActions(void) {
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
	groupTableView->setTabKeyNavigation(true);
	statusLabel->hide();
}

void Group::updateModels(void) {
	groupModel->paginator();
	qDebug() << "Query:" << groupModel->query().lastQuery();

	int begin = groupModel->begin();
	int limit = groupModel->limit();
	int sizeAll = groupModel->count();
	int size = groupModel->query().size();

	statusNotebookTableViewLabel->setText(sizeAll > 1 ? QString(qApp->tr(
			"%1 groups")).arg(sizeAll) : QString(qApp->tr("%1 group")).arg(
			sizeAll));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(begin).arg(limit).arg(
			sizeAll));

	latestPushButton->setEnabled(begin > 0);
	nextPushButton->setEnabled(begin > 0);
	previousPushButton->setEnabled(sizeAll > (size + begin));
	oldestPushButton->setEnabled(sizeAll > (size + begin));
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
	updateModels();
}

void Group::updateStatus(const QString &msg, int code) {
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
