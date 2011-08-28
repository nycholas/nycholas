/**
 * @(#)ClientNtp.java 1.0 2009/08/30
 * 
 * A simple example in client ntp socket.
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
package org.cenobites.code.java.socket.examples.one.client;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.Socket;

/**
 * Client socket ntp (port 13)
 *
 * @package org.cenobites.code.java.socket.examples.one.client
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class ClientNtp {
	public static void main(String[] args) throws Exception {		
		if (args.length != 1) {
			System.out.println("usage: java ClientNtp <host>");
			System.exit(0);
		}
		
		String host = args[0];
		int port = 13;
		
		System.out.printf(" + connecting %s:%d... %n", host, port);
		
		Socket so = new Socket(host, port);
		InputStreamReader in = new InputStreamReader(so.getInputStream());
		BufferedReader buff = new BufferedReader(in);
		
		System.out.printf(" + content... %n");

		String line = buff.readLine();
		
		System.out.printf(" + time: %s %n", line);
		
		System.out.println(" + close...");
		
		so.close();
	}
}
