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
#include "xxx.h"

Xxx::Xxx(QWidget *parent) :
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

Xxx::~Xxx(void) {
}

void Xxx::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void Xxx::newAction(void) {
	xXxModel->setId(0);
	XxxForm *form = new XxxForm(xXxModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
}

void Xxx::activateAction(void) {
	QModelIndex index = xXxTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = xXxModel->record(index.row());
	int id = record.value(xxx_id).toInt();

	XxxModel *m = new XxxModel(0, this);
	XxxModel::selectById(id, m);
	m->setDateChanged(QDateTime::currentDateTime());
	m->setIsActive(1);
	if (!m->status()) {
		errorStatus(qApp->tr("Failure trying to register the record."));
		return;
	}
	okStatus(qApp->tr("Successfully changed xxx."));
	updateModels();
}

void Xxx::desactivateAction(void) {
	QModelIndex index = xXxTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = xXxModel->record(index.row());
	int id = record.value(xxx_id).toInt();

	XxxModel *m = new XxxModel(0, this);
	XxxModel::selectById(id, m);
	m->setDateChanged(QDateTime::currentDateTime());
	m->setIsActive(0);
	if (!m->status()) {
		errorStatus(qApp->tr("Failure trying to register the record."));
		return;
	}
	okStatus(qApp->tr("Successfully changed xxx."));
	updateModels();
}

void Xxx::removeAction(void) {
	QModelIndex index = xXxTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = xXxModel->record(index.row());
	int id = record.value(xxx_id).toInt();
	QString name = record.value(xxx_name).toString();

	QMessageBox msgBox;
	msgBox.setText("Are you sure?");
	msgBox.setInformativeText(QString(qApp->tr(
			"Are you sure you want to delete the selected xxx objects?\n"
				"All of the following objects and their related items will be "
				"deleted:\n\nXxx: %1\n").arg(name)));
	msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
			| QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::Yes);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	XxxModel *m = new XxxModel(0, this);
	XxxModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted xxx."));
	updateModels();
}

void Xxx::createSearchAdvanceWidget(void) {
	xXxSearch = new XxxSearch(xXxModel);
	connect(xXxSearch, SIGNAL(formSearched()), this, SLOT(
			updateSearchForm()));
	connect(xXxSearch, SIGNAL(formSearchClose()), this, SLOT(
			updateSearchFormClose()));
}

void Xxx::searchAdvancedAction(void) {
	if (!xXxSearch->isVisible()) {
		xXxSearch->show();
		xXxSearch->raise();
		xXxSearch->activateWindow();
	} else {
		xXxSearch->hide();
	}
}

void Xxx::searchTextChangedAction() {
	QString text = searchLineEdit->text();
	if (text.isEmpty() || text.isNull()) {
		xXxModel->setF("");
	} else {
		xXxModel->setF(QString("name LIKE '%1\%'").arg(text));
	}
	xXxModel->setBegin(0);
	updateModels();
}

void Xxx::closeAction(void) {
	close();
}

void Xxx::selectedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = xXxModel->record(index.row());
	int id = record.value(xxx_id).toInt();
	xXxModel->setId(id);

	XxxForm *form = new XxxForm(xXxModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
}

void Xxx::lastestAction(void) {
	xXxModel->setBegin(0);
	updateModels();
}

void Xxx::nextAction(void) {
	xXxModel->setBegin(xXxModel->begin() - xXxModel->limit());
	updateModels();
}

void Xxx::previousAction(void) {
	xXxModel->setBegin(xXxModel->begin() + xXxModel->limit());
	updateModels();
}

void Xxx::oldestAction(void) {
	int count = xXxModel->count();
	int limit = xXxModel->limit();
	xXxModel->setBegin((limit * count / limit) - limit);
	updateModels();
}

void Xxx::createModels(void) {
	xXxModel = new XxxModel(0, this);
	xXxModel->setTable("x_x_x");
	xXxModel->setHeaderData(xxx_id, Qt::Horizontal, qApp->tr("Id"));
	xXxModel->setHeaderData(xxx_name, Qt::Horizontal,
			qApp->tr("Name"));
	xXxModel->setHeaderData(xxx_description, Qt::Horizontal,
			qApp->tr("Description"));
	xXxModel->setSort(xxx_id, Qt::DescendingOrder);
}

void Xxx::createViews(void) {
	xXxTableView->setModel(xXxModel);
	xXxTableView->setItemDelegate(new QSqlRelationalDelegate(
			xXxTableView));
	xXxTableView->setSelectionMode(QAbstractItemView::SingleSelection);
	xXxTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	xXxTableView->setColumnHidden(xxx_dateJoined, true);
	xXxTableView->setColumnHidden(xxx_dateChanged, true);
	xXxTableView->setColumnHidden(xxx_isActive, true);
	xXxTableView->resizeColumnsToContents();
	xXxTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);

	QHeaderView *header = xXxTableView->horizontalHeader();
	header->setStretchLastSection(true);
	connect(header, SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)), this,
		SLOT(updateModels()));
}

void Xxx::createActions(void) {
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

	connect(xXxTableView, SIGNAL(activated(const QModelIndex &)), this,
			SLOT(selectedItemViewAction(const QModelIndex &)));

	connect(latestPushButton, SIGNAL(released()), this, SLOT(lastestAction()));
	connect(nextPushButton, SIGNAL(released()), this, SLOT(nextAction()));
	connect(previousPushButton, SIGNAL(released()), this,
			SLOT(previousAction()));
	connect(oldestPushButton, SIGNAL(released()), this, SLOT(oldestAction()));
}

void Xxx::updateWidgets(void) {
	if (xXxModel->query().size() > 0) {
		activatePushButton->show();
		deactivatePushButton->show();
		removePushButton->show();
	} else {
		activatePushButton->hide();
		deactivatePushButton->hide();
		removePushButton->hide();
	}
	xXxTableView->setTabKeyNavigation(true);
	statusLabel->hide();
}

void Xxx::updateModels(void) {
	xXxModel->paginator();
	qDebug() << "Query:" << xXxModel->query().lastQuery();

	int begin = xXxModel->begin();
	int limit = xXxModel->limit();
	int sizeAll = xXxModel->count();
	int size = xXxModel->query().size();

	statusXxxTableViewLabel->setText(sizeAll > 1 ? QString(qApp->tr(
			"%1 xxxs")).arg(sizeAll)
			: QString(qApp->tr("%1 xxx")).arg(sizeAll));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(begin).arg(limit).arg(
			sizeAll));

	latestPushButton->setEnabled(begin > 0);
	nextPushButton->setEnabled(begin > 0);
	previousPushButton->setEnabled(sizeAll > (size + begin));
	oldestPushButton->setEnabled(sizeAll > (size + begin));
}

void Xxx::timerStatus(void) {
	statusTimer->start(3000);
}

void Xxx::okStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: green; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid green;");
	statusLabel->show();
	timerStatus();
}

void Xxx::infoStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: blue; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid blue;");
	statusLabel->show();
	timerStatus();
}

void Xxx::alertStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: yellow; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid yellow;");
	statusLabel->show();
	timerStatus();
}

void Xxx::errorStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: red; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid red;");
	statusLabel->show();
	timerStatus();
}

void Xxx::updateSearchForm(void) {
	searchLineEdit->clear();
	updateModels();
}

void Xxx::updateSearchFormClose(void) {
	searchLineEdit->clear();
	updateModels();
}

void Xxx::updateStatus(const QString &msg, int code) {
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
