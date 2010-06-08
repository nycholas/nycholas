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
	//statusTimer = new QTimer(this);
	createModels();
	createDelegates();
	createViews();
	createActions();
	updateWidgets();
}

Notebook::~Notebook(void) {
}

void Notebook::newAction(void) {
	NotebookForm *form = new NotebookForm();
	connect(form, SIGNAL(formAdded()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formChanged()), this, SLOT(updateModels()));
	connect(form, SIGNAL(formDeleted()), this, SLOT(updateModels()));
	form->show();
}

void Notebook::activateAction(void) {
	/*QModelIndexList selecteds = notebookTableView->selectedIndexes();
	QSqlDatabase::database().transaction();
	for (int i = 0; i < selecteds.size(); i++) {
		QModelIndex index = selecteds.at(i);
		if (index.isValid()) {
			QSqlRecord record = notebookModel->record(index.row());
			int id = record.value(notebook_id).toInt();

			NotebookModel *m = new NotebookModel(this);
			NotebookModel::selectById(id, m);
			m->setDateChanged(QDateTime::currentDateTime());
			m->setIsActive(1);
			if (!m->status()) {
				QMessageBox::warning(0, qApp->tr("Notebook changed"), QString(
						qApp->tr("Failure trying to register the record.")),
						QMessageBox::Ok);
				QSqlDatabase::database().rollback();
				return;
			}
		}
	}
	QSqlDatabase::database().commit();
	QMessageBox::information(0, qApp->tr("Notebook changed"), QString(qApp->tr(
			"Successfully changed %1 notebook.")).arg(selecteds.size()),
			QMessageBox::Ok);*/
}

void Notebook::desactivateAction(void) {
	/*QModelIndexList selecteds = notebookTableView->selectedIndexes();
	QSqlDatabase::database().transaction();
	for (int i = 0; i < selecteds.size(); i++) {
		QModelIndex index = selecteds.at(i);
		if (index.isValid()) {
			QSqlRecord record = notebookModel->record(index.row());
			int id = record.value(notebook_id).toInt();

			NotebookModel *m = new NotebookModel(this);
			NotebookModel::selectById(id, m);
			m->setDateChanged(QDateTime::currentDateTime());
			m->setIsActive(0);
			if (!m->status()) {
				QMessageBox::warning(0, qApp->tr("Notebook changed"), QString(
						qApp->tr("Failure trying to register the record.")),
						QMessageBox::Ok);
				QSqlDatabase::database().rollback();
				return;
			}
		}
	}
	QSqlDatabase::database().commit();
	QMessageBox::information(0, qApp->tr("Notebook changed"), QString(qApp->tr(
			"Successfully changed %1 notebook.")).arg(selecteds.size()),
			QMessageBox::Ok);*/
}

void Notebook::removeAction(void) {
	/*QModelIndexList selecteds = notebookTableView->selectedIndexes();
	QSqlDatabase::database().transaction();
	for (int i = 0; i < selecteds.size(); i++) {
		QModelIndex index = selecteds.at(i);
		if (index.isValid()) {
			QSqlRecord record = notebookModel->record(index.row());
			int id = record.value(notebook_id).toInt();

			NotebookModel *m = new NotebookModel(this);
			NotebookModel::selectById(id, m);
			if (!m->remove()) {
				QMessageBox::warning(0, qApp->tr("Notebook deleted"), QString(
						qApp->tr("Fails to remove the record.")),
						QMessageBox::Ok);
				QSqlDatabase::database().rollback();
				return;
			}
		}
	}
	QSqlDatabase::database().commit();
	QMessageBox::information(0, qApp->tr("Notebook deleted"), QString(qApp->tr(
			"Successfully deleted %1 notebook.")).arg(selecteds.size()),
			QMessageBox::Ok);*/
}

void Notebook::searchAction(void) {
}

void Notebook::closeAction(void) {
	close();
}

void Notebook::doubleClickedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		QMessageBox::warning(0, qApp->tr("No items."), qApp->tr(
				"Please select an item to edit."), QMessageBox::Ok);
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

void Notebook::createModels(void) {
	notebookModel = new QSqlRelationalTableModel(this);
	notebookModel->setTable("notebook");
	notebookModel->setHeaderData(notebook_id, Qt::Horizontal, qApp->tr("Id"));
	notebookModel->setHeaderData(notebook_name, Qt::Horizontal,
			qApp->tr("Name"));
	notebookModel->setHeaderData(notebook_description, Qt::Horizontal,
			qApp->tr("Description"));
	notebookModel->setSort(notebook_id, Qt::DescendingOrder);
	notebookModel->select();
}

void Notebook::createDelegates(void) {
	notebookDelegate = new QSqlRelationalDelegate(notebookTableView);
}

void Notebook::createViews(void) {
	notebookTableView->setModel(notebookModel);
	notebookTableView->setItemDelegate(notebookDelegate);
	notebookTableView->setSelectionMode(QAbstractItemView::ExtendedSelection);
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
	connect(newPushButton, SIGNAL(released()), this, SLOT(newAction()));
	connect(activatePushButton, SIGNAL(released()), this,
			SLOT(activateAction()));
	connect(deactivatePushButton, SIGNAL(released()), this,
			SLOT(desactivateAction()));
	connect(removePushButton, SIGNAL(released()), this, SLOT(removeAction()));
	connect(searchLineEdit, SIGNAL(editingFinished()), this,
			SLOT(searchAction()));
	connect(closePushButton, SIGNAL(released()), this, SLOT(closeAction()));

	connect(notebookTableView, SIGNAL(doubleClicked(const QModelIndex &)),
			this, SLOT(doubleClickedItemViewAction(const QModelIndex &)));
}

void Notebook::updateWidgets(void) {
	activatePushButton->hide();
	deactivatePushButton->hide();
	removePushButton->hide();
	statusLabel->hide();
}

void Notebook::updateModels(void) {
	notebookModel->select();
}
