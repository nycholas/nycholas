/**
 * @(#)MyClientMultithreaded 1.0 2009/09/01
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
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

/**
 * Client socket multithreaded
 * 
 * @package hell.cenobites.code.java.socket.examples.one.client
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class MyClientMultithreaded implements Runnable {
	private String host;
	private int port;

	public MyClientMultithreaded(String host, int port) {
		if (host == null || host.equals("") || port <= 0) {
			System.out.println("Parameters invalid");
			System.exit(-1);
		}

		this.host = host;
		this.port = port;
	}

	@Override
	public void run() {
		System.out.printf(" + connecting %s:%d... %n", host, port);

		Socket so;
		try {
			so = new Socket(host, port);

			DataOutputStream out = new DataOutputStream(so.getOutputStream());
			InputStreamReader in = new InputStreamReader(so.getInputStream());
			BufferedReader buff = new BufferedReader(in);

			String line;
			while ((line = buff.readLine()) != null) {
				System.out.println(line);
			}

			System.out.println(" + close...");
			so.close();
		} catch (IOException e) {
			System.out.println("Exp: " + e.getMessage());
			System.exit(-1);
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 2) {
			System.out.println("usage: java MyClient <host> <port>");
			System.exit(0);
		}

		String host = args[0];
		int port = Integer.valueOf(args[1]);

		MyClientMultithreaded myClient;
		for (int i = 0; i < 10; i++) {
			myClient = new MyClientMultithreaded(host, port);
			Thread th = new Thread(myClient);
			th.start();
		}
	}
}
