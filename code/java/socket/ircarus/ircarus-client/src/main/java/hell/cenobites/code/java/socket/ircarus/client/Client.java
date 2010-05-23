/**
 * @(#)Client.java 1.0 2009/09/01
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
package hell.cenobites.code.java.socket.ircarus.client;

import hell.cenobites.code.java.socket.ircarus.client.action.ReadMessageFromServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * TODO:
 * 
 * @package hell.cenobites.code.java.socket.ircarus.client
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class Client {
	private Socket socket = null;
	private PrintWriter out = null;
	private BufferedReader in = null;

	public Client(String host, int port) throws IOException {
		System.out.println(" + connecting on " + host + ":" + port);

		try {
			this.socket = new Socket(host, port);
			this.out = new PrintWriter(this.socket.getOutputStream(), true);
			this.in = new BufferedReader(new InputStreamReader(this.socket
					.getInputStream()));
		} catch (UnknownHostException e) {
			System.err.println("Don't know about host: " + host + ":" + port
					+ ".");
			System.exit(1);
		} catch (IOException e) {
			System.err.println("Couldn't get I/O for the connection to: "
					+ host + ":" + port + ".");
			System.exit(1);
		}

		BufferedReader stdIn = new BufferedReader(new InputStreamReader(
				System.in));

		// Salute!
		System.out.println(" + server: " + this.in.readLine());

		// Read messages from server
		new Thread(new ReadMessageFromServer(this.in)).start();

		String inputLine;
		while (true) {
			System.out.print(" + you: ");
			inputLine = stdIn.readLine();
			if (inputLine != null) {
				this.out.println(inputLine);

				if (inputLine.equals("/quit")) {
					break;
				}
			}
		}
	}
}
