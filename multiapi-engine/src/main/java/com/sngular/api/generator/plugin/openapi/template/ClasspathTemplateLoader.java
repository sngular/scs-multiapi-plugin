/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.template;

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

  private static final List<String> TEMPLATE_FILES = List.of(TemplateIndexConstants.TEMPLATE_INTERFACE_API, TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA,
                                                             TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK, TemplateIndexConstants.TEMPLATE_CALL_WEB_API,
                                                             TemplateIndexConstants.TEMPLATE_WEB_CLIENT, TemplateIndexConstants.TEMPLATE_CALL_REST_API,
                                                             TemplateIndexConstants.TEMPLATE_REST_CLIENT, TemplateIndexConstants.TEMPLATE_REACTIVE_API,
                                                             TemplateIndexConstants.TEMPLATE_MODEL_EXCEPTION);

  private static final List<String> TEMPLATE_AUTH_FILES = List.of(TemplateIndexConstants.TEMPLATE_API_KEY, TemplateIndexConstants.TEMPLATE_AUTHENTICATION,
                                                                  TemplateIndexConstants.TEMPLATE_HTTP_BASIC, TemplateIndexConstants.TEMPLATE_HTTP_BEARER,
                                                                  TemplateIndexConstants.TEMPLATE_OAUTH, TemplateIndexConstants.TEMPLATE_OAUTH_FLOW);

  private static final List<String> TEMPLATE_ANNOTATION_FILES = List.of(TemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION, TemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION,
                                                                        TemplateIndexConstants.TEMPLATE_MAX_ANNOTATION, TemplateIndexConstants.TEMPLATE_MAX_VALIDATOR_ANNOTATION,
                                                                        TemplateIndexConstants.TEMPLATE_MIN_ANNOTATION, TemplateIndexConstants.TEMPLATE_MIN_VALIDATOR_ANNOTATION);
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
    TEMPLATE_FILES.forEach(templateFile -> {
      try {
        templates.put(templateFile, readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/openapi/" + templateFile)).getContent()));
      } catch (final IOException e) {
        e.printStackTrace();
      }
    });
    TEMPLATE_AUTH_FILES.forEach(templateAuthFile -> {
      try {
        templates.put(templateAuthFile, readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/openapi/authTemplates/" + templateAuthFile)).getContent()));
      } catch (final IOException e) {
        e.printStackTrace();
      }
    });
    TEMPLATE_ANNOTATION_FILES.forEach(templateAnnotationFile -> {
      try {
        templates.put(templateAnnotationFile, readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/customannotations/" + templateAnnotationFile)).getContent()));
      } catch (final IOException e) {
        e.printStackTrace();
      }
    });

    return templates;
  }

  private String readFile(final InputStream file) throws IOException {
    return new String(file.readAllBytes());
  }
}
