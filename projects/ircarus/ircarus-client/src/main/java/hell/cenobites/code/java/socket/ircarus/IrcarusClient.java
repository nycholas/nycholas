/**
 * @(#)IrcarusClient.java 1.0 2009/09/01
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
package hell.cenobites.code.java.socket.ircarus;

import hell.cenobites.code.java.socket.ircarus.client.Client;

import java.io.IOException;

/**
 * TODO:
 * 
 * @package hell.cenobites.code.java.socket.ircarus
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class IrcarusClient {
	public static void main(String[] args) {
		if (args.length != 2) {
			System.err.println("usage: IrcarusClient <host> <port>");
			System.exit(-1);
		}
		
		String host = args[0];
		int port = Integer.valueOf(args[1]).intValue();
		
		try {
			Client client = new Client(host, port);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
