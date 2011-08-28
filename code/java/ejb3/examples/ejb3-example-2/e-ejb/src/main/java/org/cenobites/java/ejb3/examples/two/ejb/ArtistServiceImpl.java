/**
 * @(#)ArtistServiceImpl.java 1.0 2009/09/09
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
package org.cenobites.java.ejb3.examples.two.ejb;

import org.cenobites.java.ejb3.examples.two.ejb.client.IArtistService;
import org.cenobites.java.ejb3.examples.two.model.Artist;

import java.util.List;

import javax.ejb.Remote;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * TODO:
 * 
 * @package org.cenobites.java.ejb3.examples.two.ejb
 * @author Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * @version 1.0
 */
@Stateless(name = IArtistService.BEAN_NAME)
@Remote(IArtistService.class)
public class ArtistServiceImpl implements IArtistService {

	private static final Log log = LogFactory.getLog(ArtistServiceImpl.class);

	@PersistenceContext(name = "ePersistence")
	private EntityManager em;

	@Override
	public int add(Artist artist) {
		log.info(this.getClass().getName() + " add...");
		log.info(" + id: " + artist.getId());
		log.info(" + name: " + artist.getName());
		log.info(" + created: " + artist.getCreated());
		log.info(" + updated: " + artist.getUpdated());
		log.info(" + status: " + artist.getStatus());

		this.em.persist(artist);

		return artist.getId();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Artist> findAll() {
		log.info(this.getClass().getName() + " findAll...");

		List artistAll = this.em.createQuery("from Artist").getResultList();

		return artistAll;
	}

	@Override
	public Artist findById(int id) {
		log.info(this.getClass().getName() + " findById: " + id + "...");

		return this.em.find(Artist.class, id);
	}

	@Override
	public void remove(Artist artist) {
		log.info(this.getClass().getName() + " remove...");
		log.info(" + id: " + artist.getId());
		log.info(" + name: " + artist.getName());
		log.info(" + created: " + artist.getCreated());
		log.info(" + updated: " + artist.getUpdated());
		log.info(" + status: " + artist.getStatus());

		this.em.remove(artist);
	}

	@Override
	public void update(Artist artist) {
		log.info(this.getClass().getName() + " update...");
		log.info(" + id: " + artist.getId());
		log.info(" + name: " + artist.getName());
		log.info(" + created: " + artist.getCreated());
		log.info(" + updated: " + artist.getUpdated());
		log.info(" + status: " + artist.getStatus());

		this.em.merge(artist);
	}
}
