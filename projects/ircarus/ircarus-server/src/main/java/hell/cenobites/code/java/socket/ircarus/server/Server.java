/**
 * @(#)IrcarusServer.java 1.0 2009/09/01
 * 
 * Irc server in Java.
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
package hell.cenobites.code.java.socket.ircarus.server;

import hell.cenobites.code.java.socket.ircarus.server.action.ServerDispatch;
import hell.cenobites.code.java.socket.ircarus.server.entity.Client;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * TODO:
 * 
 * @package hell.cenobites.code.java.socket.ircarus.server
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class Server {
	private ServerSocket serv = null;
	private boolean isServ = true;

	public Server(int port) {
		try {
			System.out.println(" + listen on port: " + port);
			this.serv = new ServerSocket(port);
		} catch (IOException e) {
			System.err.println("Could not listen on port: " + port);
			System.exit(-1);
		}
		
		ServerDispatch serverDispatch = new ServerDispatch();
		serverDispatch.start();

		while (this.isServ) {
			try {
				Socket socket = this.serv.accept();
				
				Client client = new Client();
				client.setSocket(socket);
				//client.setSender(sender);
				
				serverDispatch.addClient(client);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		try {
			if (this.serv != null) {
				this.serv.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
