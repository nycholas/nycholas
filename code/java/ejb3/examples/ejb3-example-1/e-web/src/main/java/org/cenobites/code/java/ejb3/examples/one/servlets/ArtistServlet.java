/**
 * @(#)ArtistServlet.java 2.0 2011/08/28
 * 
 * A simple example ejb3.
 * Copyright (c) 2009-2011, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *  * Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
 *    its contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.cenobites.code.java.ejb3.examples.one.servlets;

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

import org.cenobites.java.ejb3.examples.one.domain.Artist;
import org.cenobites.java.ejb3.examples.one.ejb.api.ArtistServiceRemote;

public class ArtistServlet extends HttpServlet {

	private static final long serialVersionUID = -8004818354081231625L;

	private ArtistServiceRemote artistService;

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		PrintWriter out = resp.getWriter();
		out.println("Hai Hai!");
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
			this.artistService = (ArtistServiceRemote) context
					.lookup(ArtistServiceRemote.REMOTE_JNDI_NAME);
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
