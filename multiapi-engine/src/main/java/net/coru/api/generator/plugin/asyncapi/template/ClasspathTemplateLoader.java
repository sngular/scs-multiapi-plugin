/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.template;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import freemarker.cache.TemplateLoader;

public class ClasspathTemplateLoader implements TemplateLoader {

  private static final List<String> TEMPLATE_FILES = List.of("templateSuppliers.ftlh", "interfaceConsumer.ftlh", "templateConsumers.ftlh",
                                                             "interfaceSupplier.ftlh", "templateStreamBridge.ftlh");

  private static final ClassLoader LOADER = ClasspathTemplateLoader.class.getClassLoader();

  private final Map<String, String> templatesMap = new HashMap<>();

  public ClasspathTemplateLoader() {
    templatesMap.putAll(getResourceFolderFiles());
  }

  @Override
  public final Object findTemplateSource(final String templateName) {
    return templatesMap.get(templateName);
  }

  @Override
  public final long getLastModified(final Object o) {
    return 0;
  }

  @Override
  public final Reader getReader(final Object template, final String charSet) {
    return new StringReader(template.toString());
  }

  @Override
  public void closeTemplateSource(final Object o) {
    // Not required to implement
  }

  private Map<String, String> getResourceFolderFiles() {
    final Map<String, String> templates = new HashMap<>();
    try {
      for (var templateFile : TEMPLATE_FILES) {
        templates.put(templateFile,
                      readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/asyncapi/" + templateFile)).getContent()));
      }
    } catch (final IOException e) {
      e.printStackTrace();
    }
    return templates;

  }

  private String readFile(final InputStream file) throws IOException {
    return new String(file.readAllBytes());
  }
}
