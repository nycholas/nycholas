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
package org.cenobites.code.java.ejb3.examples.one.servlets;

import org.cenobites.java.ejb3.examples.one.ejb.remote.IArtistServiceRemote;
import org.cenobites.java.ejb3.examples.one.model.Artist;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Date;
import java.util.List;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * TODO:
 * 
 * @package org.cenobites.code.java.ejb3.examples.two.servlets
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
public class ArtistServlet extends HttpServlet {

	private static final long serialVersionUID = -8004818354081231625L;

	private IArtistServiceRemote artistService;

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		PrintWriter out = resp.getWriter();
		out.println("All gone to org!");
	}

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		String action = req.getParameter("action");
		String page = req.getParameter("page");
		String name = req.getParameter("name");

		Context context;
		try {
			context = new InitialContext();
			this.artistService = (IArtistServiceRemote) context
					.lookup(IArtistServiceRemote.REMOTE_JNDI_NAME);
		} catch (NamingException e) {
			throw new RuntimeException(e);
		}

		if ("add".equalsIgnoreCase(action)) {
			Artist artist = new Artist();
			artist.setName(name);
			artist.setCreated(new Date());
			artist.setStatus(true);

			this.add(artist);
		} else if ("update".equalsIgnoreCase(action)) {
			;
		} else if ("remove".equalsIgnoreCase(action)) {
			;
		} else if ("view".equalsIgnoreCase(action)) {
			;
		} else if ("list".equalsIgnoreCase(action)) {
			;
		}

		String url = this.getPage(page);
		req.setAttribute("pageInclude", url);
		RequestDispatcher dispatcher = req.getRequestDispatcher(url);
		dispatcher.forward(req, resp);
	}

	private String getPage(String page) {
		String pageDefault = "index.jsp";
		if (page == null || "".equals(page)) {
			return pageDefault;
		}
		String path = "pages/";
		String pathFile = path + page + ".jsp";
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
