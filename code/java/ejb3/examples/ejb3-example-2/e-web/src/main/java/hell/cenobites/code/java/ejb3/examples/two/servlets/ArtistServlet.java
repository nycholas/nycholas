/**
 * @(#)ArtistServlet.java 1.0 2009/09/14
 * 
 * A simple example EJB3.
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
package hell.cenobites.code.java.ejb3.examples.two.servlets;

import hell.cenobites.java.ejb3.examples.two.ejb.client.IArtistService;
import hell.cenobites.java.ejb3.examples.two.model.Artist;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Date;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.ws.WebServiceRef;

/**
 * TODO:
 * 
 * @package hell.cenobites.code.java.ejb3.examples.two.servlets
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class ArtistServlet extends HttpServlet {

	private static final long serialVersionUID = -8004818354081231625L;

	@WebServiceRef(name = IArtistService.REMOTE_JNDI_NAME)
	private IArtistService artistService;
	
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		PrintWriter out = resp.getWriter();
	    out.println("All gone to Hell!");
	}

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		String choice = req.getParameter("choice");

		if ("add".equalsIgnoreCase(choice)) {
			Artist artist = new Artist();
			artist.setName("Slayer");
			artist.setCreated(new Date());
			artist.setStatus(true);

			this.add(artist);
		} else if ("update".equalsIgnoreCase(choice)) {
			;
		} else if ("remove".equalsIgnoreCase(choice)) {
			;
		} else if ("view".equalsIgnoreCase(choice)) {
			;
		} else if ("list".equalsIgnoreCase(choice)) {
			;
		}

		String url = this.getPage(choice);
		RequestDispatcher dispatcher = req.getRequestDispatcher(url);
		dispatcher.forward(req, resp);
	}

	private String getPage(String choice) {
		String pageDefault = "index.jsp";
		if (choice == null || "".equals(choice)) {
			return pageDefault;
		}
		String path = "pages/";
		String pathFile = path + choice + ".jsp";
		File file = new File(pathFile);
		if (file.exists()) {
			return pathFile;
		}
		return pageDefault;
	}

	private void add(Artist artist) {
		this.artistService.add(artist);
	}

	public void update(Artist artist) {
		this.artistService.update(artist);
	}

	public void remove(Artist artist) {
		this.artistService.remove(artist);
	}

	public Artist findById(int id) {
		return this.artistService.findById(id);
	}

	public List<Artist> findAll() {
		return this.artistService.findAll();
	}
}
