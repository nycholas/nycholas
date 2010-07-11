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

PermissionForm::PermissionForm(QDialog *parent) :
	QDialog(parent) {
	setupUi(this);
	permissionModel = new PermissionModel();
	statusTimer = new QTimer(this);
	createActions();
	updateWidgets();
}

PermissionForm::PermissionForm(int id, QDialog *parent) :
	QDialog(parent) {
	setupUi(this);
	permissionModel = new PermissionModel();
	permissionModel->setId(id);
	statusTimer = new QTimer(this);
	createActions();
	updateWidgets();
}

PermissionForm::~PermissionForm(void) {
}

void PermissionForm::timerStatusAction(void) {
	statusLabel->hide();
	statusLabel->setText("");
}

void PermissionForm::saveAction(void) {
	if (!save()) {
		errorStatus(qApp->tr("Failure trying to register the record."));
	} else {
		if (permissionModel->getId() > 0) {
			QMessageBox::information(
					0,
					qApp->tr("Permission changed"),
					QString(qApp->tr(
							"The permission \"%1\" was changed successfully.")).arg(
							permissionModel->getName()), QMessageBox::Ok);
			emit formChanged();
		} else {
			QMessageBox::information(
					0,
					qApp->tr("Permission added"),
					QString(qApp->tr(
							"The permission \"%1\" was added successfully.")).arg(
							permissionModel->getName()), QMessageBox::Ok);
			emit formAdded();
		}
		updateModels();
		updateForms();
		close();
	}
}

void PermissionForm::saveAndContinueSavingAction(void) {
	if (!save()) {
		errorStatus(qApp->tr("Failure trying to register the record."));
	} else {
		if (permissionModel->getId() > 0) {
			okStatus(QString(qApp->tr(
					"The permission \"%1\" was changed successfully.")).arg(
					permissionModel->getName()));
			emit formChanged();
		} else {
			okStatus(QString(qApp->tr(
					"The permission \"%1\" was added successfully.")).arg(
					permissionModel->getName()));
			emit formAdded();
		}
		updateModels();
		updateForms();
	}
}

void PermissionForm::removeAction(void) {
	QMessageBox msgBox;
	msgBox.setText(qApp->tr("Are you sure?"));
	msgBox.setInformativeText(
			QString(qApp->tr(
					"Are you sure you want to delete the selected permission objects?\n"
						"All of the following objects and their related items will be "
						"deleted:\n\nPermission: %1\n").arg(
					permissionModel->getName())));
	msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
			| QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::No);
	int ret = msgBox.exec();
	if (ret == QMessageBox::Cancel) {
		updateModels();
		updateForms();
		close();
		return;
	} else if (ret == QMessageBox::No)
		return;
	if (!remove()) {
		errorStatus(qApp->tr("Fails to remove the record."));
	} else {
		QMessageBox::information(0, qApp->tr("Permission deleted"), QString(
				qApp->tr("Successfully deleted %1 permission.")).arg("1"),
				QMessageBox::Ok);
		emit
		formDeleted();
		updateModels();
		updateForms();
		close();
	}
}

void PermissionForm::cancelAction(void) {
	close();
}

void PermissionForm::createActions(void) {
	connect(statusTimer, SIGNAL(timeout()), this, SLOT(timerStatusAction()));

	connect(savePushButton, SIGNAL(released()), this, SLOT(saveAction()));
	connect(saveAndContinueSavingPushButton, SIGNAL(released()), this,
			SLOT(saveAndContinueSavingAction()));
	connect(removePushButton, SIGNAL(released()), this, SLOT(removeAction()));
	connect(cancelPushButton, SIGNAL(released()), this, SLOT(cancelAction()));
}

void PermissionForm::updateWidgets(void) {
	setWindowFlags(Qt::Dialog);
	updateForms();
	statusLabel->hide();
}

void PermissionForm::updateModels(void) {
	permissionModel = new PermissionModel();
}

void PermissionForm::updateForms(void) {
	clear();
	focusDefault();
	updateContentTypesComboBox();
	if (permissionModel->getId() > 0) {
		select();
		titleLabel->setText(qApp->tr("Change Permission"));
		formGroupBox->setTitle(qApp->tr("&Permission details"));
		savePushButton->setText(qApp->tr("&Update"));
		saveAndContinueSavingPushButton->setText(qApp->tr(
				"Update and &continue saving"));
		removePushButton->show();
	} else {
		titleLabel->setText(qApp->tr("Add Permission"));
		formGroupBox->setTitle(qApp->tr("&Permission details"));
		savePushButton->setText(qApp->tr("&Save"));
		saveAndContinueSavingPushButton->setText(qApp->tr(
				"Save and &continue saving"));
		removePushButton->hide();
	}
	savePushButton->setDefault(true);
}

void PermissionForm::updateContentTypesComboBox(void) {
	QSqlRelationalTableModel *contentTypesModel = new QSqlRelationalTableModel(this);
	contentTypesModel->setTable("app_content_type");
	contentTypesModel->setHeaderData(2, Qt::Horizontal, trUtf8("Nome"));
	contentTypesModel->setSort(2, Qt::AscendingOrder);
	contentTypesModel->select();
	contentTypesComboBox->setModel(contentTypesModel);
}

void PermissionForm::select(void) {
	PermissionModel::selectById(permissionModel->getId(), permissionModel);
	nameLineEdit->setText(permissionModel->getName());
	codenameLineEdit->setText(permissionModel->getCodename());
}

bool PermissionForm::save(void) {
	permissionModel->setName(nameLineEdit->text());
	permissionModel->setCodename(codenameLineEdit->text());
	return permissionModel->save();
}

bool PermissionForm::remove(void) {
	return permissionModel->remove();
}

void PermissionForm::clear(void) {
	nameLineEdit->clear();
	codenameLineEdit->clear();
}

void PermissionForm::focusDefault(void) {
	nameLineEdit->setFocus();
}

void PermissionForm::timerStatus(void) {
	statusTimer->start(3000);
}

void PermissionForm::okStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: green; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid green;");
	statusLabel->show();
	timerStatus();
}

void PermissionForm::infoStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: blue; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid blue;");
	statusLabel->show();
	timerStatus();
}

void PermissionForm::alertStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: yellow; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid yellow;");
	statusLabel->show();
	timerStatus();
}

void PermissionForm::errorStatus(const QString &msg) {
	statusLabel->setText(msg);
	statusLabel->setStyleSheet("color: red; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid red;");
	statusLabel->show();
	timerStatus();
}
