/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.model.SchemaObject;

import java.nio.file.Path;
import java.util.*;

public class MapperContentUtil {


  private MapperContentUtil() {
  }

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
