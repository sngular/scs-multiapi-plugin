/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.template;

import com.sngular.api.generator.plugin.common.template.CommonTemplateLoader;
import com.sngular.api.generator.plugin.exception.GeneratorTemplateException;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ClasspathTemplateLoader extends CommonTemplateLoader {


  private static final List<String> TEMPLATE_FILES = List.of("templateSuppliers.ftlh", "interfaceConsumer.ftlh", "templateConsumers.ftlh",
                                                             "interfaceSupplier.ftlh", "templateStreamBridge.ftlh",
          "interfaceSupplierWithKafkaBindings.ftlh", "templateSuppliersWithKafkaBindings.ftlh",
                                                             "interfaceConsumerWithKafkaBindings.ftlh", "templateConsumersWithKafkaBindings.ftlh",
                                                             "templateStreamBridgeWithKafkaBindings.ftlh", "templateMessageWrapper.ftlh");
  public ClasspathTemplateLoader() {
    super();
    init(getResourceFolderFiles());
  }

  private Map<String, String> getResourceFolderFiles() {
    final Map<String, String> templates = new HashMap<>();
    try {
      for (var templateFile : TEMPLATE_MODEL_FILES) {
        templates.put(templateFile,
                readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/model/" + templateFile)).getContent()));
      }
      for (var templateFile : TEMPLATE_FILES) {
        templates.put(templateFile,
                      readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/asyncapi/" + templateFile)).getContent()));
      }
      for (var templateFile : TEMPLATE_ANNOTATION_FILES) {
        templates.put(templateFile,
                      readFile((InputStream) Objects.requireNonNull(LOADER.getResource("templates/customannotations/" + templateFile)).getContent()));
      }
    } catch (final IOException e) {
      throw new GeneratorTemplateException("Template Engine error", e);
    }
    return templates;

  }
}
