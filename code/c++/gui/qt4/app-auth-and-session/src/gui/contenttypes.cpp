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
#include "contenttypes.h"

ContentTypes::ContentTypes(QWidget *parent) :
	QWidget(parent) {
	setupUi(this);
	statusTimer = new QTimer(this);
	createModels();
	createViews();
	createActions();
	updateModels();
	updateWidgets();
}

ContentTypes::~ContentTypes(void) {
}

void ContentTypes::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void ContentTypes::newAction(void) {
	ContentTypesForm *form = new ContentTypesForm();
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->exec();
}

void ContentTypes::activateAction(void) {
	qDebug() << "In ContentTypes::activateAction()";
	qDebug() << "Not implemented";
}

void ContentTypes::desactivateAction(void) {
	qDebug() << "In ContentTypes::desactivateAction()";
	qDebug() << "Not implemented";
}

void ContentTypes::removeAction(void) {
	QModelIndex index = contentTypesTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = contentTypesModel->record(index.row());
	int id = record.value(contenttype_id).toInt();
	QString name = record.value(contenttype_name).toString();

	QMessageBox msgBox;
	msgBox.setText("Are you sure?");
	msgBox.setInformativeText(QString(qApp->tr(
			"Are you sure you want to delete the selected content types objects?\n"
				"All of the following objects and their related items will be "
				"deleted:\n\nContent Types: %1\n").arg(name)));
	msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
			| QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::No);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	ContentTypesModel *m = new ContentTypesModel();
	ContentTypesModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted content types."));
	updateModels();
}

void ContentTypes::searchAdvancedAction(bool checked) {
	if (checked) {
		contentTypesSearch = new ContentTypesSearch(contentTypesModel);
		contentTypesSearch->setAttribute(Qt::WA_DeleteOnClose);
		connect(contentTypesSearch, SIGNAL(formSearched()), this, SLOT(
				updateSearchForm()));
		connect(contentTypesSearch, SIGNAL(formSearchClose()), this, SLOT(
				updateSearchFormClose()));
		//connect(contentTypesSearch, SIGNAL(hide()), this, SLOT(formSearchClose()));
		contentTypesSearch->exec();
	} else {
		contentTypesSearch->close();
	}
}

void ContentTypes::searchTextChangedAction(const QString &text) {
	if (text.isEmpty() || text.isNull()) {
		contentTypesModel->setFilter("");
	} else {
		contentTypesModel->setFilter(QString("name LIKE '%1\%'").arg(text));
	}
	updateModels();
}

void ContentTypes::doubleClickedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = contentTypesModel->record(index.row());
	int id = record.value(contenttype_id).toInt();

	ContentTypesForm *form = new ContentTypesForm(id);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->exec();
}

void ContentTypes::lastestAction(void) {
	contentTypesModel->query().first();
	updateModels();
}

void ContentTypes::nextAction(void) {
	contentTypesModel->query().next();
	updateModels();
}

void ContentTypes::previousAction(void) {
	contentTypesModel->query().previous();
	updateModels();
}

void ContentTypes::oldestAction(void) {
	contentTypesModel->query().last();
	updateModels();
}

void ContentTypes::createModels(void) {
	contentTypesModel = new QSqlRelationalTableModel(this);
	contentTypesModel->setTable("app_content_type");
	contentTypesModel->setHeaderData(contenttype_id, Qt::Horizontal, qApp->tr(
			"Id"));
	contentTypesModel->setHeaderData(contenttype_name, Qt::Horizontal,
			qApp->tr("Name"));
	contentTypesModel->setHeaderData(contenttype_appLabel, Qt::Horizontal,
			qApp->tr("App Label"));
	contentTypesModel->setHeaderData(contenttype_model, Qt::Horizontal,
			qApp->tr("Model"));
	contentTypesModel->setSort(contenttype_id, Qt::DescendingOrder);
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
}

void ContentTypes::createActions(void) {
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

	connect(contentTypesTableView, SIGNAL(doubleClicked(const QModelIndex &)),
			this, SLOT(doubleClickedItemViewAction(const QModelIndex &)));

	connect(latestPushButton, SIGNAL(released()), this, SLOT(lastestAction()));
	connect(nextPushButton, SIGNAL(released()), this, SLOT(nextAction()));
	connect(previousPushButton, SIGNAL(released()), this,
			SLOT(previousAction()));
	connect(oldestPushButton, SIGNAL(released()), this, SLOT(oldestAction()));
}

void ContentTypes::updateWidgets(void) {
	if (contentTypesModel->query().size() > 0) {
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

void ContentTypes::updateModels(void) {
	contentTypesModel->select();
	qDebug() << "Query:" << contentTypesModel->query().lastQuery();

	int size = contentTypesModel->query().size() < 0 ? 0
			: contentTypesModel->query().size();
	statusTableViewLabel->setText(size > 1 ? QString(qApp->tr(
			"%1 content types")).arg(size) : QString(qApp->tr(
			"%1 content types")).arg(size));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(1).arg(25).arg(size));
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
	searchAdvancedToolButton->setChecked(false);
	updateModels();
}
