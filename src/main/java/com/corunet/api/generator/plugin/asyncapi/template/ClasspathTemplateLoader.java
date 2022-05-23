/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.asyncapi.template;

import freemarker.cache.TemplateLoader;

import java.io.*;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ClasspathTemplateLoader implements TemplateLoader {

  private static final List<String> templateFiles = List.of("templateSuppliers.ftlh", "interfaceConsumer.ftlh", "templateConsumers.ftlh",
                                                            "interfaceSupplier.ftlh", "templateStreamBridge.ftlh");

  private final Map<String, String> templatesMap = new HashMap<>();

  private static final ClassLoader loader = ClasspathTemplateLoader.class.getClassLoader();

  public ClasspathTemplateLoader() {

    try {
      templatesMap.putAll(getResourceFolderFiles());
    } catch (URISyntaxException e) {
      e.printStackTrace();
    }
  }

  @Override
  public Object findTemplateSource(String templateName) {
    return templatesMap.get(templateName);
  }

  @Override
  public long getLastModified(Object o) {
    return 0;
  }

  @Override
  public Reader getReader(Object template, String charSet) {
    return new StringReader(template.toString());
  }

  @Override
  public void closeTemplateSource(Object o) {
    // Not required to implement
  }

  private Map<String, String> getResourceFolderFiles() throws URISyntaxException {
    Map<String, String> templates = new HashMap<>();
    try {
      for (var templateFile : templateFiles) {
        templates.put(templateFile,
                      readFile((InputStream) Objects.requireNonNull(loader.getResource("templates/functional/" + templateFile)).getContent()));
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
    return templates;

  }

  private String readFile(InputStream file) throws IOException {
    return new String(file.readAllBytes());
  }
}
