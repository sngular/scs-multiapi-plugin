/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.ModelBuilder;
import org.apache.commons.lang3.StringUtils;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class MapperContentUtil {


  private MapperContentUtil() {
  }

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final JsonNode model,
      final String schemaName, final CommonSpecFile specFile, final Path baseDir) {

    return mapComponentToSchemaObject(totalSchemas, compositedSchemas, new HashSet<>(), model,
                                      StringUtils.defaultIfEmpty(ApiTool.getName(model), schemaName), specFile, baseDir);
  }

  private static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final Set<String> antiLoopList, final JsonNode schema, final String schemaName, final CommonSpecFile specFile,
      final Path baseDir) {
    final var name = StringUtils.defaultIfBlank(ApiTool.getName(schema), schemaName);
    if (!compositedSchemas.containsKey(name)) {
      compositedSchemas
        .put(name, ModelBuilder.buildSchemaObject(totalSchemas, schemaName, schema, antiLoopList, compositedSchemas,"", specFile, baseDir));
    }
    return compositedSchemas;
  }

}
