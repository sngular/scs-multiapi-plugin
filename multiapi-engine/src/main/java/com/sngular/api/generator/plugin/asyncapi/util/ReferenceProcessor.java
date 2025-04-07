package com.sngular.api.generator.plugin.asyncapi.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import lombok.Builder;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public final class ReferenceProcessor {

  private static final String JSON = "json";

  private static final String YML = "yml";

  private static final String YAML = "yaml";

  private static final String REF = "$ref";

  private static final String AVSC = "avsc";

  private List<String> alreadyProcessed;

  private final FileLocation ymlParent;

  private final Map<String, JsonNode> totalSchemas;

  @Builder
  public ReferenceProcessor(final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas) {
    this.ymlParent = ymlParent;
    this.totalSchemas = totalSchemas;
  }

  public void processReference(final JsonNode node, final String referenceLink) {
    if (alreadyProcessed == null) {
      alreadyProcessed = new ArrayList<>();
    }
    final String[] path = MapperUtil.splitReference(referenceLink);
    final JsonNode component;
    final var calculatedKey = MapperUtil.getRefSchemaKey(referenceLink);
    if (!totalSchemas.containsKey(calculatedKey) && !alreadyProcessed.contains(calculatedKey)) {
      alreadyProcessed.add(calculatedKey);
      try {
        if (referenceLink.toLowerCase().contains(YML) || referenceLink.toLowerCase().contains(YAML) || referenceLink.toLowerCase().contains(JSON)) {
          component = solveRef(ymlParent, path[0], path[1], totalSchemas);
        } else {
          if (referenceLink.toLowerCase().contains(AVSC)) {
            component = solveRef(ymlParent, path[0], path[1], totalSchemas);
          } else {
            component = node.at(MapperUtil.getPathToModel(referenceLink)).get(MapperUtil.getModel(referenceLink));
          }
          if (Objects.nonNull(component)) {
            checkReference(node, component);
          }
        }
      } catch (final IOException e) {
        throw new FileSystemException(e);
      }
      if (Objects.nonNull(component)) {
        totalSchemas.put(calculatedKey, component);
      }
    }
  }

  private JsonNode solveRef(final FileLocation ymlParent, final String path, final String reference, final Map<String, JsonNode> totalSchemas) throws IOException {
    JsonNode returnNode = null;

    if (path.endsWith(YML) || path.endsWith(JSON) || path.endsWith(YAML)) {
      final JsonNode node = ApiTool.nodeFromFile(ymlParent, path, FactoryTypeEnum.YML);
      if (node.at(MapperUtil.getPathToModel(reference)).has(MapperUtil.getModel(reference))) {
        returnNode = node.at(MapperUtil.getPathToModel(reference)).get(MapperUtil.getModel(reference));
        checkReference(node, returnNode);
      } else {
        returnNode = node;
      }
    } else if (path.endsWith(AVSC)) {
      returnNode = ApiTool.nodeFromFile(ymlParent, path, FactoryTypeEnum.AVRO);
    } else {
      returnNode = totalSchemas.getOrDefault(MapperUtil.getRefSchemaKey(reference), null);
    }
    return returnNode;
  }

  private void checkReference(
      final JsonNode mainNode, final JsonNode node) {
    final var localReferences = node.findValues(REF);
    if (!localReferences.isEmpty()) {
      localReferences.forEach(localReference -> processReference(mainNode, ApiTool.getNodeAsString(localReference)));
    }
  }
}
