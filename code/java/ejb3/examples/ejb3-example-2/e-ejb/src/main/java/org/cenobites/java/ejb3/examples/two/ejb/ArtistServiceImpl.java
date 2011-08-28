/**
 * @(#)ArtistServiceImpl.java 2.0 2011/08/28
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
package org.cenobites.java.ejb3.examples.two.ejb;

import java.util.List;

import javax.ejb.Remote;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.cenobites.java.ejb3.examples.two.domain.Artist;
import org.cenobites.java.ejb3.examples.two.ejb.client.ArtistService;

@Stateless(name = ArtistService.BEAN_NAME)
@Remote(ArtistService.class)
public class ArtistServiceImpl implements ArtistService {

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

		@SuppressWarnings("rawtypes")
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
