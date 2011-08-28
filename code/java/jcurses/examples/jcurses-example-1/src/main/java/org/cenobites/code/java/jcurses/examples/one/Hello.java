/**
 * @(#)Hello.java 1.0 2009/08/29
 * 
 * A simple example in JCurses.
 * Copyright (C) 2009 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
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
 */
package org.cenobites.code.java.jcurses.examples.one;

import jcurses.system.CharColor;
import jcurses.widgets.DefaultLayoutManager;
import jcurses.widgets.Label;
import jcurses.widgets.WidgetsConstants;
import jcurses.widgets.Window;

/**
 * Simple example in JCurses
 * 
 * @package org.cenobites.code.java.jcurses.examples.one
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class Hello {
	public static void main(String[] args) throws Exception {
		Window w = new Window(100, 100, true, "Hai hai!");
		Label label = new Label("Hai hai!", new CharColor(CharColor.BLACK,
				CharColor.GREEN));
		DefaultLayoutManager mgr = new DefaultLayoutManager();
		mgr.bindToContainer(w.getRootPanel());
		mgr.addWidget(label, 0, 0, 100, 100, WidgetsConstants.ALIGNMENT_CENTER,
				WidgetsConstants.ALIGNMENT_CENTER);
		w.show();
		Thread.currentThread();
		Thread.sleep(3000);
		w.close();
	}
}
