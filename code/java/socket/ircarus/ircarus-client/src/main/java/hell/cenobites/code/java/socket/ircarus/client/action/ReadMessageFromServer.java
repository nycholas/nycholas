/**
 * @(#)ReadMessageFromServer.java 1.0 2009/09/01
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
package hell.cenobites.code.java.socket.ircarus.client.action;

import java.io.BufferedReader;
import java.io.IOException;

/**
 * TODO:
 * 
 * @package hell.cenobites.code.java.socket.ircarus.client.action
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class ReadMessageFromServer implements Runnable {
	private BufferedReader in;

	public ReadMessageFromServer(BufferedReader in) {
		this.in = in;
	}

	@Override
	public void run() {
		String inputLine = null;
		try {
			while ((inputLine = this.in.readLine()) != null) {
				System.out.println(" + server: " + inputLine);
				if (inputLine.equals("Bye.")) {
					break;
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
