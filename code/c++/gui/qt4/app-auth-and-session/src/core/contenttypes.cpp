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
#include "contenttypes.h"

ContentTypes::ContentTypes(QWidget *parent) :
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

ContentTypes::~ContentTypes(void) {
}

void ContentTypes::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void ContentTypes::newAction(void) {
	contentTypesModel->setId(0);
	ContentTypesForm *form = new ContentTypesForm(contentTypesModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
}

void ContentTypes::activateAction(void) {
	qDebug() << "In GroupModel::status()";
	qDebug() << "Not implemented";
}

void ContentTypes::desactivateAction(void) {
	qDebug() << "In GroupModel::status()";
	qDebug() << "Not implemented";
}

void ContentTypes::removeAction(void) {
	QModelIndex index = contentTypesTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = contentTypesModel->record(index.row());
	int id = record.value(contenttypes_id).toInt();
	QString name = record.value(contenttypes_name).toString();

	QMessageBox msgBox;
	msgBox.setText("Are you sure?");
	msgBox.setInformativeText(QString(qApp->tr(
			"Are you sure you want to delete the selected contenttypes objects?\n"
				"All of the following objects and their related items will be "
				"deleted:\n\nContent Types: %1\n").arg(name)));
	msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
			| QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::Yes);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	ContentTypesModel *m = new ContentTypesModel(0, this);
	ContentTypesModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted contenttypes."));
	updateModels();
}

void ContentTypes::createSearchAdvanceWidget(void) {
	contentTypesSearch = new ContentTypesSearch(contentTypesModel);
	connect(contentTypesSearch, SIGNAL(formSearched()), this, SLOT(
			updateSearchForm()));
	connect(contentTypesSearch, SIGNAL(formSearchClose()), this, SLOT(
			updateSearchFormClose()));
}

void ContentTypes::searchAdvancedAction(void) {
	if (!contentTypesSearch->isVisible()) {
		contentTypesSearch->show();
		contentTypesSearch->raise();
		contentTypesSearch->activateWindow();
	} else {
		contentTypesSearch->hide();
	}
}

void ContentTypes::searchTextChangedAction() {
	QString text = searchLineEdit->text();
	if (text.isEmpty() || text.isNull()) {
		contentTypesModel->setF("");
	} else {
		contentTypesModel->setF(QString("name LIKE '%1\%'").arg(text));
	}
	contentTypesModel->setBegin(0);
	updateModels();
}

void ContentTypes::closeAction(void) {
	close();
}

void ContentTypes::selectedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = contentTypesModel->record(index.row());
	int id = record.value(contenttypes_id).toInt();
	contentTypesModel->setId(id);

	ContentTypesForm *form = new ContentTypesForm(contentTypesModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
}

void ContentTypes::lastestAction(void) {
	contentTypesModel->setBegin(0);
	updateModels();
}

void ContentTypes::nextAction(void) {
	contentTypesModel->setBegin(contentTypesModel->begin()
			- contentTypesModel->limit());
	updateModels();
}

void ContentTypes::previousAction(void) {
	contentTypesModel->setBegin(contentTypesModel->begin()
			+ contentTypesModel->limit());
	updateModels();
}

void ContentTypes::oldestAction(void) {
	int count = contentTypesModel->count();
	int limit = contentTypesModel->limit();
	contentTypesModel->setBegin((limit * count / limit) - limit);
	updateModels();
}

void ContentTypes::createModels(void) {
	contentTypesModel = new ContentTypesModel(0, this);
	contentTypesModel->setTable("app_content_type");
	contentTypesModel->setHeaderData(contenttypes_id, Qt::Horizontal, qApp->tr(
			"Id"));
	contentTypesModel->setHeaderData(contenttypes_name, Qt::Horizontal,
			qApp->tr("Name"));
	contentTypesModel->setHeaderData(contenttypes_appLabel, Qt::Horizontal,
			qApp->tr("App Label"));
	contentTypesModel->setHeaderData(contenttypes_model, Qt::Horizontal,
			qApp->tr("Model"));
	contentTypesModel->setSort(contenttypes_id, Qt::DescendingOrder);
}

void ContentTypes::createViews(void) {
	contentTypesTableView->setModel(contentTypesModel);
	contentTypesTableView->setItemDelegate(new QSqlRelationalDelegate(
			contentTypesTableView));
	contentTypesTableView->setSelectionMode(QAbstractItemView::SingleSelection);
	contentTypesTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	contentTypesTableView->resizeColumnsToContents();
	contentTypesTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);

	QHeaderView *header = contentTypesTableView->horizontalHeader();
	header->setStretchLastSection(true);
connect(header, SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)), this,
		SLOT(updateModels()));
}

void ContentTypes::createActions(void) {
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

	connect(contentTypesTableView, SIGNAL(activated(const QModelIndex &)), this,
			SLOT(selectedItemViewAction(const QModelIndex &)));

	connect(latestPushButton, SIGNAL(released()), this, SLOT(lastestAction()));
	connect(nextPushButton, SIGNAL(released()), this, SLOT(nextAction()));
	connect(previousPushButton, SIGNAL(released()), this,
			SLOT(previousAction()));
	connect(oldestPushButton, SIGNAL(released()), this, SLOT(oldestAction()));
}

void ContentTypes::updateWidgets(void) {
	if (contentTypesModel->query().size() > 0) {
		activatePushButton->hide();
		deactivatePushButton->hide();
		removePushButton->show();
	} else {
		activatePushButton->hide();
		deactivatePushButton->hide();
		removePushButton->hide();
	}
	contentTypesTableView->setTabKeyNavigation(true);
	statusLabel->hide();
}

void ContentTypes::updateModels(void) {
	contentTypesModel->paginator();
	qDebug() << "Query:" << contentTypesModel->query().lastQuery();

	int begin = contentTypesModel->begin();
	int limit = contentTypesModel->limit();
	int sizeAll = contentTypesModel->count();
	int size = contentTypesModel->query().size();

	statusContentTypesTableViewLabel->setText(sizeAll > 1 ? QString(qApp->tr(
			"%1 content typess")).arg(sizeAll) : QString(qApp->tr(
			"%1 content types")).arg(sizeAll));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(begin).arg(limit).arg(
			sizeAll));

	latestPushButton->setEnabled(begin > 0);
	nextPushButton->setEnabled(begin > 0);
	previousPushButton->setEnabled(sizeAll > (size + begin));
	oldestPushButton->setEnabled(sizeAll > (size + begin));
}

void ContentTypes::timerStatus(void) {
	statusTimer->start(3000);
}

void ContentTypes::okStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: green; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid green;");
	statusLabel->show();
	timerStatus();
}

void ContentTypes::infoStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: blue; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid blue;");
	statusLabel->show();
	timerStatus();
}

void ContentTypes::alertStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: yellow; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid yellow;");
	statusLabel->show();
	timerStatus();
}

void ContentTypes::errorStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: red; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid red;");
	statusLabel->show();
	timerStatus();
}

void ContentTypes::updateSearchForm(void) {
	searchLineEdit->clear();
	updateModels();
}

void ContentTypes::updateSearchFormClose(void) {
	searchLineEdit->clear();
	updateModels();
}

void ContentTypes::updateStatus(const QString &msg, int code) {
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
