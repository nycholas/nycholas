/**
 * Simple example Qt - CRUD.
 * Copyright (C) 2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 **/
#include <QApplication>

#include "commons/connection.h"
#include "gui/notebook.h"

int main(int argc, char *argv[]) {
	QApplication app(argc, argv);
	if (!createConnection())
		return 1;
	//app.connect(app, SIGNAL(lastWindowClosed()), SLOT(closeConnection()));
	Notebook *window = new Notebook();
	window->show();
	return app.exec();
}

