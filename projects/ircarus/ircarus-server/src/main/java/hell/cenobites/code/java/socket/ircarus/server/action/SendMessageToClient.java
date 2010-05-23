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

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * TODO:
 * 
 * @package hell.cenobites.code.java.socket.ircarus.server.action
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class SendMessageToClient extends Thread {
	private Client client;
	private PrintWriter out;
	private List<String> messages = new ArrayList<String>();

	public SendMessageToClient(Client client) {
		this.client = client;
		try {
			this.out = new PrintWriter(new OutputStreamWriter(client
					.getSocket().getOutputStream()));

			this.sendMessageToClient(" :: Welcome to hell! :: ");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public synchronized void sendMessage(String message) {
		this.messages.add(message);
		notify();
	}

	public void sendMessageToClient(String message) {
		this.out.println(message);
		this.out.flush();
	}

	@Override
	public void run() {
		while (!this.isInterrupted()) {
			String message = this.getNextMessageFromQueue();
			this.sendMessageToClient(message);
		}
	}

	private synchronized String getNextMessageFromQueue() {
		while (this.messages.size() == 0) {
			try {
				wait();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		String message = (String) this.messages.get(0);
		this.messages.remove(0);
		return message;
	}

}
