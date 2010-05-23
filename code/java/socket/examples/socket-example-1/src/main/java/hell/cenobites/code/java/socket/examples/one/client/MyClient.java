/**
 * @(#)MyClient.java 1.0 2009/08/30
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
package hell.cenobites.code.java.socket.examples.one.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

/**
 * Client socket
 * 
 * @package hell.cenobites.code.java.socket.examples.one.client
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class MyClient {
	private Socket so;

	public MyClient(String host, int port) {
		if (host == null || host.equals("") || port <= 0) {
			System.out.println("Parameters invalid");
			System.exit(-1);
		}

		System.out.printf(" + connecting %s:%d... %n", host, port);

		try {
			this.so = new Socket(host, port);
		} catch (IOException e) {
			System.out.println("Exp: " + e.getMessage());
			System.exit(-1);
		}
	}

	public void listen() {
		try {
			PrintWriter out = new PrintWriter(this.so.getOutputStream(), true);
			BufferedReader in = new BufferedReader(new InputStreamReader(
					this.so.getInputStream()));

			BufferedReader stdIn = new BufferedReader(new InputStreamReader(
					System.in));

			String fromServer;
			String fromClient;
			
			out.println("Hello, my master!");
			
			while ((fromServer = in.readLine()) != null) {
				System.out.println(" + server: " + fromServer);
				
				fromClient = stdIn.readLine();
				if (fromClient != null) {
					out.println(fromClient);
				}
			}

			stdIn.close();
			
			out.close();
			in.close();
		} catch (IOException e) {
			System.out.println("Exp: " + e.getMessage());
			System.exit(-1);
		} finally {
			System.out.println(" + close...");
			try {
				this.so.close();
			} catch (IOException e) {
				System.out.println("Exp: " + e.getMessage());
				System.exit(-1);
			}
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 2) {
			System.out.println("usage: java MyClient <host> <port>");
			System.exit(0);
		}

		String host = args[0];
		int port = Integer.valueOf(args[1]);

		MyClient myClient = new MyClient(host, port);
		myClient.listen();
	}
}
