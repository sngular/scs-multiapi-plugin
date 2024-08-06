/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.util;


import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.tools.ModelBuilder;

import java.nio.file.Path;
import java.util.*;

public class MapperContentUtil {

  private static final String ADDITIONAL_PROPERTY_NAME = "AdditionalProperty";

  private static final String ADDITIONAL_PROPERTIES = "additionalProperties";

  private static final String ANY_OF_COMBINATOR = "anyOf";

  private static final String ONE_OF_COMBINATOR = "oneOf";

  private static final String ALL_OF_COMBINATOR = "allOf";

  private MapperContentUtil() {}

  public static List<SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas, final String className, final JsonNode model,
      final String parentPackage, final CommonSpecFile specFile, final Path baseDir) {
    final List<SchemaObject> schemasList = new ArrayList<>();
    if (Objects.nonNull(model)) {
      final var modelToBuildMap = new HashMap<String, SchemaObject>();
      schemasList.add(ModelBuilder.buildSchemaObject(totalSchemas, className, model, new HashSet<>(), modelToBuildMap, parentPackage, specFile, baseDir));
      schemasList.addAll(modelToBuildMap.values());
    }
    return schemasList;
  }


}
