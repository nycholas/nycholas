/**
 * @(#)MyAction.java 1.0 2009/08/30
 * 
 * A simple example in Socket Client/Server.
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
package org.cenobites.code.java.socket.examples.one.server.action;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

/**
 * Socket action in requisition
 * 
 * @package org.cenobites.code.java.socket.examples.one.server.action
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class MyAction implements Runnable {
	private Socket sock;

	public MyAction(Socket sock) {
		this.sock = sock;

		Thread th = Thread.currentThread();
		System.out.println(" + start " + th.getName() + "...");
	}

	@Override
	public void run() {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(
					this.sock.getInputStream()));
			PrintWriter out = new PrintWriter(this.sock.getOutputStream(), true);

			out.println("Welcome to Hell");

			String fromClient = null;
			while ((fromClient = in.readLine()) != null) {
				System.out
						.println(" + client["
								+ Thread.currentThread().getName() + "]: "
								+ fromClient);
				
				out.println("What's?");

				if (fromClient.equalsIgnoreCase("/quit")) {
					System.out.println(" + stop["
							+ Thread.currentThread().getName() + "]");
					break;
				}
			}

			out.println(" :: BYE :: ");

			out.close();
			in.close();
		} catch (Exception e) {
			System.out.println("Exp: " + e.getMessage());
			System.exit(-1);
		}
	}
}
