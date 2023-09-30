package com.sngular.api.generator.plugin.common.tools;

import java.nio.file.Path;
import java.util.Map;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.openapi.utils.MapperUtil;
import com.sngular.api.generator.plugin.openapi.utils.OpenApiUtil;
import org.apache.commons.lang3.StringUtils;

public class SchemaUtil {

  protected SchemaUtil() {
  }

  public static JsonNode solveRef(final String refValue, final Map<String, JsonNode> schemaMap, final Path rootFilePath) {
    JsonNode solvedRef;
    if (StringUtils.isNotEmpty(refValue)) {
      if (refValue.startsWith("#")) {
        final String refSchemaName = MapperUtil.getRefSchemaName(refValue);
        solvedRef = schemaMap.get(refSchemaName);
      } else {
        final var refValueArr = refValue.split("#");
        final var filePath = refValueArr[0];
        solvedRef = OpenApiUtil.getPojoFromRef(rootFilePath.toAbsolutePath(), filePath);
        final var refName = MapperUtil.getRefSchemaName(refValueArr[1]);
        schemaMap.putAll(ApiTool.getComponentSchemas(solvedRef));
        solvedRef = solvedRef.findValue(refName);
      }
    } else {
      solvedRef = null;
    }
    return solvedRef;
  }
}
