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
	createModels();
	createDelegates();
	createViews();
	createActions();
	updateWidgets();
}

Notebook::~Notebook(void) {
}

void Notebook::newAction(void) {
	NotebookItem *notebookItem = new NotebookItem();
	connect(notebookItem, SIGNAL(tableListChanged()), this,
			SLOT(updateModels()));
	notebookItem->show();
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

	QSqlRecord record = model->record(index.row());
	int id = record.value(notebook_id).toInt();

	NotebookItem *notebookItem = new NotebookItem(id);
	connect(notebookItem, SIGNAL(tableListChanged()), this,
			SLOT(updateModels()));
	notebookItem->show();
}

void Notebook::createModels(void) {
	model = new QSqlRelationalTableModel(this);
	model->setTable("notebook");
	model->setHeaderData(notebook_id, Qt::Horizontal, qApp->tr("Id"));
	model->setHeaderData(notebook_name, Qt::Horizontal, qApp->tr("Name"));
	model->setHeaderData(notebook_description, Qt::Horizontal, qApp->tr(
			"Description"));
	model->setSort(notebook_id, Qt::DescendingOrder);
	model->select();
}

void Notebook::createDelegates(void) {
	delegate = new QSqlRelationalDelegate(tableViewList);
}

void Notebook::createViews(void) {
	tableViewList->setModel(model);
	tableViewList->setItemDelegate(delegate);
	tableViewList->setSelectionMode(QAbstractItemView::ExtendedSelection);
	tableViewList->setSelectionBehavior(QAbstractItemView::SelectRows);
	tableViewList->setColumnHidden(notebook_dateJoined, true);
	tableViewList->setColumnHidden(notebook_dateChanged, true);
	tableViewList->setColumnHidden(notebook_isActive, true);
	tableViewList->resizeColumnsToContents();
	tableViewList->setEditTriggers(QAbstractItemView::NoEditTriggers);

	QHeaderView *header = tableViewList->horizontalHeader();
	header->setStretchLastSection(true);
}

void Notebook::createActions(void) {
	connect(pushButtonNew, SIGNAL(released()), this, SLOT(newAction()));
	//connect(pushButtonCancel, SIGNAL(released()), this, SLOT(closeAction()));

connect(tableViewList, SIGNAL(doubleClicked(const QModelIndex &)), this,
		SLOT(doubleClickedItemViewAction(const QModelIndex &)));
}

void Notebook::updateWidgets(void) {
}

void Notebook::updateModels(void) {
	model->setTable("notebook");
	model->setHeaderData(notebook_id, Qt::Horizontal, qApp->tr("Id"));
	model->setHeaderData(notebook_name, Qt::Horizontal, qApp->tr("Name"));
	model->setHeaderData(notebook_description, Qt::Horizontal, qApp->tr(
			"Description"));
	model->setSort(notebook_id, Qt::DescendingOrder);
	model->select();
}
