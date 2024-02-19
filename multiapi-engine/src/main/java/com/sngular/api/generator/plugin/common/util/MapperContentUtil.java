package com.sngular.api.generator.plugin.common.util;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.parameter.SpecFile;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.openapi.model.SchemaObject;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode schema,
      final String schemaFullName,
      final SpecFile specFile,
      final Path baseDir) {
    final Set<String> processedSchemaNames = new HashSet<>();
    String name = StringUtils.defaultIfEmpty(ApiTool.getName(schema), schemaFullName);
    return null;
  }
}
