package org.cenobites.code.java.ejb3.examples.two.servlets;

import javax.naming.InitialContext;

import org.cenobites.java.ejb3.examples.two.ejb.client.IArtistService;

public class ArtistServletTest {
	public static void main(String[] args) throws Exception {
		IArtistService service = null;
		service = (IArtistService) new InitialContext()
				.lookup(IArtistService.REMOTE_JNDI_NAME);
		service.findAll();
	}
}
