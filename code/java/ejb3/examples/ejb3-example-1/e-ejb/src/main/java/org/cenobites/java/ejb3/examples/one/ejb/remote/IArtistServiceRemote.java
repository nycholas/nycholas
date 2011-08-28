/**
 * @(#)IArtistServiceRemote.java 1.0 2009/09/09
 * 
 * A simple example ejb3.
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
package org.cenobites.java.ejb3.examples.one.ejb.remote;

import org.cenobites.java.ejb3.examples.one.model.Artist;

import java.util.List;

import javax.ejb.Remote;

/**
 * TODO:
 * 
 * @package org.cenobites.java.ejb3.examples.one.ejb.remote
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
@Remote
public interface IArtistServiceRemote {
	
	public final static String BEAN_NAME = "ArtistService";

	public final static String REMOTE_JNDI_NAME = "e-ear/" + BEAN_NAME + "/remote";
	
	public int add(Artist artist);

	public void update(Artist artist);

	public void remove(Artist artist);

	public Artist findById(int id);

	public List<Artist> findAll();
}
