package com.sngular.api.generator.plugin.asyncapi.util;

import static com.sngular.api.generator.plugin.common.tools.ApiTool.nodeFromFile;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedSchemaException;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import lombok.Builder;

public class ReferenceProcessor {

  private static final String JSON = "json";

  private static final String YML = "yml";

  private static final String REF = "$ref";

  private static final String AVSC = "avsc";

  private static final String SLASH = "/";

  private List<String> alreadyProcessed;

  private final FileLocation ymlParent;

  private final Map<String, JsonNode> totalSchemas;

  @Builder
  private ReferenceProcessor(FileLocation ymlParent, Map<String, JsonNode> totalSchemas) {
    this.ymlParent = ymlParent;
    this.totalSchemas = totalSchemas;
  }

  public void processReference(
      final JsonNode node, final String referenceLink) {
    if (alreadyProcessed == null) {
      alreadyProcessed = new ArrayList<>();
    }
    final String[] path = MapperUtil.splitName(referenceLink);
    final JsonNode component;
    final var calculatedKey = calculateKey(path);
    if (!totalSchemas.containsKey(calculatedKey) && !alreadyProcessed.contains(calculatedKey)) {
      alreadyProcessed.add(calculatedKey);
      try {
        if (referenceLink.toLowerCase().contains(YML) || referenceLink.toLowerCase().contains(JSON)) {
          component = solveRef(ymlParent, path, referenceLink, totalSchemas);
        } else {
          if (referenceLink.toLowerCase().contains(AVSC)) {
            component = solveRef(ymlParent, path, referenceLink, totalSchemas);
          } else {
            component = (node.findValue(path[path.length - 2])).get(path[path.length - 1]);
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

  private String calculateKey(final String[] path) {
    return (path[path.length - 2] + SLASH + path[path.length - 1]).toUpperCase();
  }

  private JsonNode solveRef(final FileLocation ymlParent, final String[] path, final String reference, final Map<String, JsonNode> totalSchemas) throws IOException {
    final String[] pathToFile = reference.split("#");
    final String filePath = pathToFile[0];
    JsonNode returnNode = null;

    if (filePath.endsWith(YML) || filePath.endsWith(JSON)) {
      final JsonNode node = nodeFromFile(ymlParent, filePath, FactoryTypeEnum.YML);
      if (node.findValue(path[path.length - 2]).has(path[path.length - 1])) {
        returnNode = node.findValue(path[path.length - 2]).get(path[path.length - 1]);
        checkReference(node, returnNode);
      } else {
        throw new NonSupportedSchemaException(node.toPrettyString());
      }
    } else if (filePath.endsWith(AVSC)) {
      returnNode = nodeFromFile(ymlParent, filePath, FactoryTypeEnum.AVRO);
    } else if (totalSchemas.containsKey((path[path.length - 2] + SLASH + path[path.length - 1]).toUpperCase())) {
      returnNode = totalSchemas.get((path[path.length - 2] + SLASH + path[path.length - 1]).toUpperCase());
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
