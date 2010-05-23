/**
 * @(#)MyServer.java 1.0 2009/08/30
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
package hell.cenobites.code.java.socket.examples.one.server;

import hell.cenobites.code.java.socket.examples.one.server.action.MyAction;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;


/**
 * Socket server multithreaded
 *
 * @package hell.cenobites.code.java.socket.examples.one.server
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class MyServer {
	final static int PORT = 6660;
	private ServerSocket serv;

	public MyServer() {
		System.out.printf(" + creating server socket %d... %n", PORT);
		try {
			this.serv = new ServerSocket(PORT);
		} catch (IOException e) {
			System.out.println("Exp: " + e.getMessage());
			System.exit(-1);
		}
	}

	public void listen() {
		System.out.println(" + listen sockets...");
		
		while (true) {
			try {
				Socket so = this.serv.accept();

				System.out.printf(" + client %s connection... %n", so
						.getLocalAddress());

				MyAction client = new MyAction(so);
				Thread th = new Thread(client);
				th.start();
			} catch (IOException e) {
				System.out.println("Exp: " + e.getMessage());
				System.exit(-1);
			}
		}
	}

	@Override
	protected void finalize() {
		try {
			this.serv.close();
		} catch (IOException e) {
			System.out.println("Exp: " + e);
			System.exit(-1);
		}
	}

	public static void main(String[] args) {
		MyServer myServer = new MyServer();
		myServer.listen();
	}
}