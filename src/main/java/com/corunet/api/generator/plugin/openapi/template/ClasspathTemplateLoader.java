/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.openapi.template;

import static com.corunet.api.generator.plugin.openapi.template.TemplateIndexConstants.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import freemarker.cache.TemplateLoader;

public class ClasspathTemplateLoader implements TemplateLoader {

  private final static List<String> templateFiles = List.of(TEMPLATE_INTERFACE_API, TEMPLATE_CONTENT_SCHEMA, TEMPLATE_CONTENT_SCHEMA_LOMBOK, TEMPLATE_CALL_WEB_API,
                                                            TEMPLATE_WEB_CLIENT, TEMPLATE_CALL_REST_API, TEMPLATE_REST_CLIENT, TEMPLATE_REACTIVE_API);

  private final static List<String> templateAuthFiles = List.of(TEMPLATE_API_KEY, TEMPLATE_AUTHENTICATION, TEMPLATE_HTTP_BASIC, TEMPLATE_HTTP_BEARER, TEMPLATE_OAUTH,
                                                                TEMPLATE_OAUTH_FLOW);

  private final Map<String, String> templatesMap = new HashMap<>();

  private final static ClassLoader loader = ClasspathTemplateLoader.class.getClassLoader();

  public ClasspathTemplateLoader() {

    try {
      templatesMap.putAll(getResourceFolderFiles());
    } catch (MalformedURLException | URISyntaxException e) {
      e.printStackTrace();
    }
  }

  @Override
  public Object findTemplateSource(String templateName) throws IOException {
    return templatesMap.get(templateName);
  }

  @Override
  public long getLastModified(Object o) {
    return 0;
  }

  @Override
  public Reader getReader(Object template, String charSet) throws IOException {
    return new StringReader(template.toString());
  }

  @Override
  public void closeTemplateSource(Object o) throws IOException {
    // Not required to implement
  }

  private Map<String, String> getResourceFolderFiles() throws MalformedURLException, URISyntaxException {
    Map<String, String> templates = new HashMap<>();
    templateFiles.forEach(templateFile -> {
      try {
        templates.put(templateFile, readFile((InputStream) Objects.requireNonNull(loader.getResource("templates/openapi/" + templateFile)).getContent()));
      } catch (IOException e) {
        e.printStackTrace();
      }
    });
    templateAuthFiles.forEach(templateAuthFile -> {
      try {
        templates.put(templateAuthFile, readFile((InputStream) Objects.requireNonNull(loader.getResource("templates/openapi/authTemplates/" + templateAuthFile)).getContent()));
      } catch (IOException e) {
        e.printStackTrace();
      }
    });

    return templates;
  }

  private String readFile(InputStream file) throws IOException {
    return new String(file.readAllBytes());
  }
}
