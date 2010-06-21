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
#include "notebook.h"

Notebook::Notebook(QWidget *parent) :
	QWidget(parent) {
	setupUi(this);
	statusTimer = new QTimer(this);
	createModels();
	createViews();
	createActions();
	updateModels();
	updateWidgets();
}

Notebook::~Notebook(void) {
}

void Notebook::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void Notebook::newAction(void) {
	NotebookForm *form = new NotebookForm();
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
}

void Notebook::activateAction(void) {
	QModelIndex index = notebookTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = notebookModel->record(index.row());
	int id = record.value(notebook_id).toInt();

	NotebookModel *m = new NotebookModel(this);
	NotebookModel::selectById(id, m);
	m->setDateChanged(QDateTime::currentDateTime());
	m->setIsActive(1);
	if (!m->status()) {
		errorStatus(qApp->tr("Failure trying to register the record."));
		return;
	}
	okStatus(qApp->tr("Successfully changed notebook."));
	updateModels();
}

void Notebook::desactivateAction(void) {
	QModelIndex index = notebookTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = notebookModel->record(index.row());
	int id = record.value(notebook_id).toInt();

	NotebookModel *m = new NotebookModel(this);
	NotebookModel::selectById(id, m);
	m->setDateChanged(QDateTime::currentDateTime());
	m->setIsActive(0);
	if (!m->status()) {
		errorStatus(qApp->tr("Failure trying to register the record."));
		return;
	}
	okStatus(qApp->tr("Successfully changed notebook."));
	updateModels();
}

void Notebook::removeAction(void) {
	QModelIndex index = notebookTableView->currentIndex();
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = notebookModel->record(index.row());
	int id = record.value(notebook_id).toInt();
	QString name = record.value(notebook_name).toString();

	QMessageBox msgBox;
	msgBox.setText("Are you sure?");
	msgBox.setInformativeText(QString(qApp->tr(
			"Are you sure you want to delete the selected notebook objects?\n"
				"All of the following objects and their related items will be "
				"deleted:\n\nNotebook: %1\n").arg(name)));
	msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
			| QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::No);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	NotebookModel *m = new NotebookModel(this);
	NotebookModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted notebook."));
	updateModels();
}

void Notebook::searchAdvancedAction(bool checked) {
	if (checked) {
		notebookSearch = new NotebookSearch(notebookModel);
		notebookSearch->setAttribute(Qt::WA_DeleteOnClose);
		connect(notebookSearch, SIGNAL(formSearched()), this,
				SLOT(updateSearchForm()));
		connect(notebookSearch, SIGNAL(formSearchClose()), this,
				SLOT(updateSearchFormClose()));
		//connect(notebookSearch, SIGNAL(hide()), this, SLOT(formSearchClose()));
		notebookSearch->show();
		notebookSearch->raise();
		notebookSearch->activateWindow();
	} else {
		notebookSearch->close();
	}
}

void Notebook::searchTextChangedAction(const QString &text) {
	if (text.isEmpty() || text.isNull()) {
		notebookModel->setFilter("");
	} else {
		notebookModel->setFilter(QString("name LIKE '%1\%'").arg(text));
	}
	updateModels();
}

void Notebook::closeAction(void) {
	close();
}

void Notebook::doubleClickedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = notebookModel->record(index.row());
	int id = record.value(notebook_id).toInt();

	NotebookForm *form = new NotebookForm(id);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
}

void Notebook::lastestAction(void) {
	notebookModel->query().first();
	updateModels();
}

void Notebook::nextAction(void) {
	notebookModel->query().next();
	updateModels();
}

void Notebook::previousAction(void) {
	notebookModel->query().previous();
	updateModels();
}

void Notebook::oldestAction(void) {
	notebookModel->query().last();
	updateModels();
}

void Notebook::createModels(void) {
	notebookModel = new QSqlRelationalTableModel(this);
	notebookModel->setTable("notebook");
	notebookModel->setHeaderData(notebook_id, Qt::Horizontal, qApp->tr("Id"));
	notebookModel->setHeaderData(notebook_name, Qt::Horizontal,
			qApp->tr("Name"));
	notebookModel->setHeaderData(notebook_description, Qt::Horizontal,
			qApp->tr("Description"));
	notebookModel->setSort(notebook_id, Qt::DescendingOrder);
}

void Notebook::createViews(void) {
	notebookTableView->setModel(notebookModel);
	notebookTableView->setItemDelegate(new QSqlRelationalDelegate(
			notebookTableView));
	notebookTableView->setSelectionMode(QAbstractItemView::SingleSelection);
	notebookTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	notebookTableView->setColumnHidden(notebook_dateJoined, true);
	notebookTableView->setColumnHidden(notebook_dateChanged, true);
	notebookTableView->setColumnHidden(notebook_isActive, true);
	notebookTableView->resizeColumnsToContents();
	notebookTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);

	QHeaderView *header = notebookTableView->horizontalHeader();
	header->setStretchLastSection(true);
}

void Notebook::createActions(void) {
	connect(statusTimer, SIGNAL(timeout()), this, SLOT(timerStatusAction()));

	connect(newPushButton, SIGNAL(released()), this, SLOT(newAction()));
	connect(activatePushButton, SIGNAL(released()), this,
			SLOT(activateAction()));
	connect(deactivatePushButton, SIGNAL(released()), this,
			SLOT(desactivateAction()));
	connect(removePushButton, SIGNAL(released()), this, SLOT(removeAction()));
	connect(searchAdvancedToolButton, SIGNAL(toggled(bool)), this,
			SLOT(searchAdvancedAction(bool)));
	connect(searchLineEdit, SIGNAL(textChanged(const QString &)), this,
			SLOT(searchTextChangedAction(const QString &)));
	connect(closePushButton, SIGNAL(released()), this, SLOT(closeAction()));

	connect(notebookTableView, SIGNAL(doubleClicked(const QModelIndex &)),
			this, SLOT(doubleClickedItemViewAction(const QModelIndex &)));

	connect(latestPushButton, SIGNAL(released()), this, SLOT(lastestAction()));
	connect(nextPushButton, SIGNAL(released()), this, SLOT(nextAction()));
	connect(previousPushButton, SIGNAL(released()), this,
			SLOT(previousAction()));
	connect(oldestPushButton, SIGNAL(released()), this, SLOT(oldestAction()));
}

void Notebook::updateWidgets(void) {
	if (notebookModel->query().size() > 0) {
		activatePushButton->show();
		deactivatePushButton->show();
		removePushButton->show();
	} else {
		activatePushButton->hide();
		deactivatePushButton->hide();
		removePushButton->hide();
	}
	statusLabel->hide();
}

void Notebook::updateModels(void) {
	notebookModel->select();
	qDebug() << "Query:" << notebookModel->query().lastQuery();

	int size = notebookModel->query().size();
	statusNotebookTableViewLabel->setText(size > 1 ? QString(qApp->tr(
			"%1 notebooks")).arg(size) : QString(qApp->tr("%1 notebook")).arg(
			size));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(1).arg(25).arg(size));
}

void Notebook::timerStatus(void) {
	statusTimer->start(3000);
}

void Notebook::okStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: green; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid green;");
	statusLabel->show();
	timerStatus();
}

void Notebook::infoStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: blue; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid blue;");
	statusLabel->show();
	timerStatus();
}

void Notebook::alertStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: yellow; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid yellow;");
	statusLabel->show();
	timerStatus();
}

void Notebook::errorStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: red; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid red;");
	statusLabel->show();
	timerStatus();
}

void Notebook::updateSearchForm(void) {
	searchLineEdit->clear();
	updateModels();
}

void Notebook::updateSearchFormClose(void) {
	searchLineEdit->clear();
	searchAdvancedToolButton->setChecked(false);
	updateModels();
}
