/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.openapi.exception.FileParseException;
import com.sngular.api.generator.plugin.openapi.exception.InvalidOpenAPIException;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.Map.Entry;

public class OpenApiUtil {

  private OpenApiUtil() {}

  public static Map<String, HashMap<String, JsonNode>> mapApiGroups(final JsonNode openAPI, final boolean groupByTags) {
    var mapApis = new HashMap<String, HashMap<String, JsonNode>>();
    final var pathList = openAPI.findValue("paths").elements();
    if (pathList.hasNext()) {
      mapApis = groupByTags ? mapApiGroupsByTags(pathList) : mapApiGroupsByUrl(openAPI);
    }

    return mapApis;
  }

  private static HashMap<String, HashMap<String, JsonNode>> mapApiGroupsByTags(final Iterator<JsonNode> pathList) {

    final var mapApis = new HashMap<String, HashMap<String, JsonNode>>();
    while (pathList.hasNext()) {
      final JsonNode openAPIPath = pathList.next();
      final var mapMethodsByTag = getMapMethodsByTag(openAPIPath);
      for (Entry<String, JsonNode> tagMethodEntry : mapMethodsByTag.entries()) {
        mapApis.compute(tagMethodEntry.getKey(), (key, value) -> initOrInsert(openAPIPath, tagMethodEntry, value));
      }
    }

    return mapApis;

  }

  private static HashMap<String, JsonNode> initOrInsert(
      final Entry<String, JsonNode> openAPIGetPathsEntry, final Entry<String, JsonNode> mapJsonNodes,
      final HashMap<String, JsonNode> value) {
    var newValue = value;
    if (Objects.isNull(newValue)) {
      newValue = new HashMap<>();
    }
    newValue.put(openAPIGetPathsEntry.getKey(), mapJsonNodes.getValue());

    return newValue;
  }

  private static MultiValuedMap<String, JsonNode> getMapMethodsByTag(final JsonNode pathItem) {
    final MultiValuedMap<String, JsonNode> mapByTag = new ArrayListValuedHashMap<>();
    final var operations = pathItem.elements();
    while (operations.hasNext()) {
      final JsonNode method = operations.next();
      final var tag = method.findValue("tags").elements().next();
      mapByTag.put(tag.asText(), method);
    }
    return mapByTag;
  }

  private static HashMap<String, Map<String, Iterator<JsonNode>>> mapApiGroupsByUrl(final JsonNode openAPI) {
    final var mapByUrl = new HashMap<String, Map<String, Iterator<JsonNode>>>();

    for (var openAPIPaths : openAPI.findValues("paths")) {
      final var pathUrl = openAPIPaths.textValue();
      final String[] pathName = pathUrl.split("/");
      mapByUrl.putIfAbsent(pathName[1], new HashMap<>());
      mapByUrl.get(pathName[1]).put(pathUrl, openAPIPaths.elements());
    }

    return mapByUrl;
  }

  public static JsonNode getPojoFromSwagger(final SpecFile specFile) {
    final JsonNode openAPI;
    final ObjectMapper parser = new ObjectMapper(new YAMLFactory());
    try {
      openAPI = parser.readTree(readFile(specFile.getFilePath()));
    } catch (final IOException e) {
      throw new FileParseException(specFile.getFilePath(), e);
    }

    if (Objects.isNull(openAPI)) {
      throw new FileParseException("empty .yml");
    }

    return openAPI;
  }

  private static String readFile(final String filePath) {
    var result = filePath;
    final URL fileURL = OpenApiUtil.class.getClassLoader().getResource(filePath);
    if (Objects.nonNull(fileURL)) {
      result = fileURL.toString();
    }
    return result;
  }

  public static Map<String, JsonNode> processBasicJsonNodes(final JsonNode openApi) {
    final var basicJsonNodeMap = new HashMap<String, JsonNode>();
    final var componentsSchemasList = new ArrayList<JsonNode>();
    if (openApi.has("components")) {
      openApi.get("components").findValue("schemas").elements().forEachRemaining(componentsSchemasList::add);
    }

    for (Iterator<JsonNode> pathElement = openApi.findValue("path").elements(); pathElement.hasNext();) {
      final var path = pathElement.next();
      switch (path.textValue()) {
        case "get":
        case "post":
        case "put":
        case "delete":
        case "patch":
          processContentForBasicJsonNodes(basicJsonNodeMap, path);
          break;
        default:
          throw new InvalidOpenAPIException();
      }
    }

    return basicJsonNodeMap;
  }

  private static void processContentForBasicJsonNodes(final HashMap<String, JsonNode> basicJsonNodeMap, final JsonNode operation) {

    processParameters(basicJsonNodeMap, operation);
    processRequestBody(basicJsonNodeMap, operation);
    processResponses(basicJsonNodeMap, operation);
  }

  private static void processRequestBody(final HashMap<String, JsonNode> basicJsonNodeMap, final JsonNode operation) {
    if (operation.has("requestBody") && Objects.nonNull(operation.at("requestBody/content"))) {
      final var content = operation.at("requestBody/content");
      final var schema = content.findValue("schema");
      if (schema.has("$ref")) {
        basicJsonNodeMap.put("InlineObject" + StringUtils.capitalize(operation.get("operationId").asText()), schema);
      } else if (schema.has("items")) {
        basicJsonNodeMap.put("InlineObject" + StringUtils.capitalize(operation.get("operationId").asText()),
                schema.get("items"));
      }
    }
  }

  private static void processResponses(final HashMap<String, JsonNode> basicJsonNodeMap, final JsonNode operation) {
    if (operation.has("responses")) {
      final var responses = operation.path("responses");
      for (Iterator<JsonNode> it = responses.elements(); it.hasNext(); ) {
        var response = it.next();
        if (response.has("content")) {
          response.getValue().getContent().forEach((key, value) -> {
            if (Objects.isNull(value.getJsonNode().get$ref()) && "object".equalsIgnoreCase(value.getJsonNode().getType())) {
              basicJsonNodeMap.put("InlineResponse" + response.getKey() + StringUtils.capitalize(operation.getJsonNodeId()),
                      value.getJsonNode());
            } else if (value.getJsonNode() instanceof ComposedJsonNode) {
              basicJsonNodeMap.put("InlineResponse" + response.getKey() + StringUtils.capitalize(operation.getJsonNodeId()) + getComposedJsonNodeName(value.getJsonNode()),
                      value.getJsonNode());
            }
          });
        }
      }
    }
  }
  private static void processParameters(final HashMap<String, JsonNode> basicJsonNodeMap, final JsonNode operation) {
        if (Objects.nonNull(operation.getParameters())) {
          for (Parameter parameter : operation.getParameters()) {
            if (Objects.nonNull(parameter.getContent())) {
              parameter.getContent()
                      .forEach((name, mediaType) -> basicJsonNodeMap.putIfAbsent(
                              "InlineParameter" + StringUtils.capitalize(operation.getJsonNodeId()) + StringUtils.capitalize(parameter.getName()),
                              mediaType.getJsonNode()));
            }
          }
        }
      }
  }

  private static String getComposedJsonNodeName(final JsonNode schema) {
    String composedJsonNodeName = "";
    if (Objects.nonNull(schema.getAllOf())) {
      composedJsonNodeName = "AllOf";
    } else if (Objects.nonNull(schema.getAnyOf())) {
      composedJsonNodeName = "AnyOf";
    } else if (Objects.nonNull(schema.getOneOf())) {
      composedJsonNodeName = "OneOf";
    }
    return composedJsonNodeName;
  }

  public static String processJavaFileName(final String apisEntry) {
    String javaFileName;
    final List<Integer> positionList = new ArrayList<>();
    if (apisEntry.contains("/")) {
      final String[] wholeApiEntry = apisEntry.split("/");
      javaFileName = capLettersAfterSpecialCharacters(wholeApiEntry[0], positionList);
    } else {
      javaFileName = capLettersAfterSpecialCharacters(apisEntry, positionList);
    }
    javaFileName = StringUtils.capitalize(javaFileName.replaceAll("[^A-Za-z0-9]", ""));
    return javaFileName;
  }

  private static String capLettersAfterSpecialCharacters(final String pathName, final List<Integer> positionList) {
    String javaFileName;
    final char[] pathAsChars = pathName.toCharArray();
    for (int i = 0; i < pathAsChars.length; i++) {
      if (!Character.isLetterOrDigit(pathAsChars[i])) {
        positionList.add(i);
      }
    }
    javaFileName = pathName;
    for (Integer position : positionList) {
      javaFileName = javaFileName.substring(0, position + 1) + javaFileName.substring(position + 1, position + 2).toUpperCase(Locale.ROOT) + javaFileName.substring(position + 2);
    }
    return javaFileName;
  }


}


