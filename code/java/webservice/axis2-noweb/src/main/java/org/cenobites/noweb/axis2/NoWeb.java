/**
 * WebService - No web.
 * Copyright (c) 2010, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
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
package org.cenobites.noweb.axis2;

import java.util.HashMap;
import java.util.Map;

import org.apache.axis2.context.ConfigurationContext;
import org.apache.axis2.context.ConfigurationContextFactory;
import org.apache.axis2.description.AxisService;
import org.apache.axis2.engine.MessageReceiver;
import org.apache.axis2.rpc.receivers.RPCMessageReceiver;
import org.apache.axis2.transport.http.SimpleHTTPServer;
import org.cenobites.service.axis2.AnyService;

public class NoWeb {
	public static void main(String[] args) throws Exception {
		Map<String, MessageReceiver> mrMap = new HashMap<String, MessageReceiver>();
		mrMap.put("http://www.w3.org/2004/08/wsdl/in-only",
				RPCMessageReceiver.class.newInstance());
		mrMap.put("http://www.w3.org/2004/08/wsdl/out-only",
				RPCMessageReceiver.class.newInstance());

		ConfigurationContext context = ConfigurationContextFactory
				.createConfigurationContextFromFileSystem(null, null);
		AxisService service = AxisService.createService(AnyService.class
				.getName(), context.getAxisConfiguration(), mrMap,
				"http://cenobites.org/target-namespace/anyservice",
				"http://cenobites.org/schema-namespace/anyservice", NoWeb.class
						.getClassLoader());
		context.getAxisConfiguration().addService(service);

		SimpleHTTPServer server = new SimpleHTTPServer(context, 8686);
		server.start();
	}
}
