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
	createSearchAdvanceWidget();
	updateWidgets();
}

Notebook::~Notebook(void) {
}

void Notebook::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void Notebook::newAction(void) {
	notebookModel->setId(0);
	NotebookForm *form = new NotebookForm(notebookModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
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

	NotebookModel *m = new NotebookModel(0, this);
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

	NotebookModel *m = new NotebookModel(0, this);
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
	msgBox.setDefaultButton(QMessageBox::Yes);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		return;
	} else if (ret == QMessageBox::No)
		return;

	NotebookModel *m = new NotebookModel(0, this);
	NotebookModel::selectById(id, m);
	if (!m->remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
		return;
	}
	okStatus(qApp->tr("Successfully deleted notebook."));
	updateModels();
}

void Notebook::createSearchAdvanceWidget(void) {
	notebookSearch = new NotebookSearch(notebookModel);
	connect(notebookSearch, SIGNAL(formSearched()), this, SLOT(
			updateSearchForm()));
	connect(notebookSearch, SIGNAL(formSearchClose()), this, SLOT(
			updateSearchFormClose()));
}

void Notebook::searchAdvancedAction(void) {
	if (!notebookSearch->isVisible()) {
		notebookSearch->show();
		notebookSearch->raise();
		notebookSearch->activateWindow();
	} else {
		notebookSearch->hide();
	}
}

void Notebook::searchTextChangedAction() {
	QString text = searchLineEdit->text();
	if (text.isEmpty() || text.isNull()) {
		notebookModel->setF("");
	} else {
		notebookModel->setF(QString("name LIKE '%1\%'").arg(text));
	}
	notebookModel->setBegin(0);
	updateModels();
}

void Notebook::closeAction(void) {
	close();
}

void Notebook::selectedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		infoStatus(qApp->tr("Please select an item to edit."));
		return;
	}

	QSqlRecord record = notebookModel->record(index.row());
	int id = record.value(notebook_id).toInt();
	notebookModel->setId(id);

	NotebookForm *form = new NotebookForm(notebookModel);
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	connect(form, SIGNAL(sendStatus(const QString &, int)), this,
			SLOT(updateStatus(const QString &, int)));
	form->show();
}

void Notebook::lastestAction(void) {
	notebookModel->setBegin(0);
	updateModels();
}

void Notebook::nextAction(void) {
	notebookModel->setBegin(notebookModel->begin() - notebookModel->limit());
	updateModels();
}

void Notebook::previousAction(void) {
	notebookModel->setBegin(notebookModel->begin() + notebookModel->limit());
	updateModels();
}

void Notebook::oldestAction(void) {
	int count = notebookModel->count();
	int limit = notebookModel->limit();
	notebookModel->setBegin((limit * count / limit) - limit);
	updateModels();
}

void Notebook::createModels(void) {
	notebookModel = new NotebookModel(0, this);
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
connect(header, SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)), this,
		SLOT(updateModels()));
}

void Notebook::createActions(void) {
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
	connect(closePushButton, SIGNAL(released()), this, SLOT(closeAction()));

	connect(notebookTableView, SIGNAL(activated(const QModelIndex &)), this,
			SLOT(selectedItemViewAction(const QModelIndex &)));

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
	notebookTableView->setTabKeyNavigation(true);
	statusLabel->hide();
}

void Notebook::updateModels(void) {
	notebookModel->paginator();
	qDebug() << "Query:" << notebookModel->query().lastQuery();

	int begin = notebookModel->begin();
	int limit = notebookModel->limit();
	int sizeAll = notebookModel->count();
	int size = notebookModel->query().size();

	statusNotebookTableViewLabel->setText(sizeAll > 1 ? QString(qApp->tr(
			"%1 notebooks")).arg(sizeAll)
			: QString(qApp->tr("%1 notebook")).arg(sizeAll));
	statusPaginationLabel->setText(QString(qApp->tr(
			"<b>%1</b> - <b>%2</b> de <b>%3</b>")).arg(begin).arg(limit).arg(
			sizeAll));

	latestPushButton->setEnabled(begin > 0);
	nextPushButton->setEnabled(begin > 0);
	previousPushButton->setEnabled(sizeAll > (size + begin));
	oldestPushButton->setEnabled(sizeAll > (size + begin));
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
	updateModels();
}

void Notebook::updateStatus(const QString &msg, int code) {
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
