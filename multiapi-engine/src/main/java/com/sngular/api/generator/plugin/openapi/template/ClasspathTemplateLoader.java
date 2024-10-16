/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.template;

import com.sngular.api.generator.plugin.common.template.CommonTemplateLoader;
import com.sngular.api.generator.plugin.exception.GeneratorTemplateException;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ClasspathTemplateLoader extends CommonTemplateLoader {

  private static final List<String> TEMPLATE_FILES = List.of(TemplateIndexConstants.TEMPLATE_INTERFACE_API, TemplateIndexConstants.TEMPLATE_CALL_WEB_API,
                                                             TemplateIndexConstants.TEMPLATE_WEB_CLIENT, TemplateIndexConstants.TEMPLATE_CALL_REST_API,
                                                             TemplateIndexConstants.TEMPLATE_REST_CLIENT, TemplateIndexConstants.TEMPLATE_REACTIVE_API);

  private static final List<String> TEMPLATE_AUTH_FILES = List.of(TemplateIndexConstants.TEMPLATE_API_KEY, TemplateIndexConstants.TEMPLATE_AUTHENTICATION,
                                                                  TemplateIndexConstants.TEMPLATE_HTTP_BASIC, TemplateIndexConstants.TEMPLATE_HTTP_BEARER,
                                                                  TemplateIndexConstants.TEMPLATE_OAUTH, TemplateIndexConstants.TEMPLATE_OAUTH_FLOW);
  public ClasspathTemplateLoader() {
    super();
    init(getResourceFolderFiles());
  }

  private Map<String, String> getResourceFolderFiles() {
    final Map<String, String> templates = new HashMap<>();
    TEMPLATE_MODEL_FILES.forEach(templateFile -> {
      try {
        templates.put(templateFile, readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/model/" + templateFile)).getContent()));
      } catch (final IOException e) {
        throw new GeneratorTemplateException("Error loading Model Templates", e);
      }
    });
    TEMPLATE_FILES.forEach(templateFile -> {
      try {
        templates.put(templateFile, readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/openapi/" + templateFile)).getContent()));
      } catch (final IOException e) {
        throw new GeneratorTemplateException("Error loading Api Templates", e);
      }
    });
    TEMPLATE_AUTH_FILES.forEach(templateAuthFile -> {
      try {
        templates.put(templateAuthFile, readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/openapi/authTemplates/" + templateAuthFile)).getContent()));
      } catch (final IOException e) {
        throw new GeneratorTemplateException("Error loading Auth Templates", e);
      }
    });
    TEMPLATE_ANNOTATION_FILES.forEach(templateAnnotationFile -> {
      try {
        templates.put(templateAnnotationFile,
                      readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/customannotations/" + templateAnnotationFile)).getContent()));
      } catch (final IOException e) {
        throw new GeneratorTemplateException("Error loading Annotation Templates", e);
      }
    });

    return templates;
  }
}
