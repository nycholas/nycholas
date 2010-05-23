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
package hell.cenobites.code.java.socket.ircarus.server.action;

import hell.cenobites.code.java.socket.ircarus.server.entity.Client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.HashSet;
import java.util.Set;

/**
 * TODO:
 * 
 * @package hell.cenobites.code.java.socket.ircarus.server.action
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class ServerDispatch extends Thread {
	private Set<Client> clients = new HashSet<Client>();
	
	private Socket socket = null;
	private BufferedReader in = null;
	private PrintWriter out = null;

	public ServerDispatch() {		
		try {
			this.socket = null;
			this.in = new BufferedReader(new InputStreamReader(this.socket
					.getInputStream()));
			this.out = new PrintWriter(this.socket.getOutputStream(), true);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void addClient(Client client) {
		this.clients.add(client);
	}
	
	public void removeClient(Client client) {
		this.clients.remove(client);
	}
	
	public synchronized void sendMessageToAllClient() {
		for (Client client : this.clients) {
			//client.
		}
	}

	@Override
	public void run() {
		Thread currTh = Thread.currentThread();

		System.out.println(" + start client[" + currTh.getName() + "]");

		try {
			this.in = new BufferedReader(new InputStreamReader(this.socket
					.getInputStream()));
			this.out = new PrintWriter(this.socket.getOutputStream(), true);

			// Salute!
			this.out.println("Welcome to Hell!");
			
			// Read messages from client
			//new Thread(new ReadMessageFromClient(currTh, this.in)).start();
			
			String inputLine;
			while ((inputLine = this.in.readLine()) != null) {
				System.out.println(" + client[" + currTh.getName() + "]: "
						+ inputLine);

				//new WriteMessageToClient(inputLine);
				
				//outputLine = "What's?";
				//this.out.println(inputLine);

				if (inputLine.equals("/quit")) {
					System.out.println(" + stop client[" + currTh.getName()
							+ "]");
					break;
				}
			}
			this.out.println("Bye.");
			this.out.close();
			this.in.close();
			this.socket.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
