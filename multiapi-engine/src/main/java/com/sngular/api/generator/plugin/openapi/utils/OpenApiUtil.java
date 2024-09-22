/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.StringCaseUtils;
import com.sngular.api.generator.plugin.openapi.exception.FileParseException;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.commons.lang3.StringUtils;

public class OpenApiUtil {

  public static final String PATHS = "paths";

  static final ObjectMapper PARSER = new ObjectMapper(new YAMLFactory());

  static final Set<String> REST_VERB_SET = Set.of("get", "post", "delete", "patch", "put");

  private OpenApiUtil() {}

  public static MultiValuedMap<String, Map<String, JsonNode>> mapApiGroups(final JsonNode openAPI, final boolean groupByTags) {
    final MultiValuedMap<String, Map<String, JsonNode>> mapApis = new ArrayListValuedHashMap<>();
    final var pathList = openAPI.findValue(PATHS).fields();
    if (pathList.hasNext()) {
      mapApis.putAll(groupByTags ? mapApiGroupsByTags(pathList) : mapApiGroupsByUrl(openAPI));
    }

    return mapApis;
  }

  private static MultiValuedMap<String, Map<String, JsonNode>> mapApiGroupsByTags(final Iterator<Entry<String, JsonNode>> pathList) {

    final MultiValuedMap<String, Map<String, JsonNode>> mapApis = new ArrayListValuedHashMap<>();
    while (pathList.hasNext()) {
      final Entry<String, JsonNode> openAPIPath = pathList.next();
      mapApis.putAll(getMapMethodsByTag(openAPIPath));
    }

    return mapApis;

  }

  private static MultiValuedMap<String, Map<String, JsonNode>> getMapMethodsByTag(final Entry<String, JsonNode> pathItem) {
    final MultiValuedMap<String, Map<String, JsonNode>> mapByTag = new ArrayListValuedHashMap<>();
    final var operations = IteratorUtils.filteredIterator(pathItem.getValue().fields(), opProperty -> REST_VERB_SET.contains(opProperty.getKey()));
    while (operations.hasNext()) {
      final var method = operations.next();
      if (ApiTool.hasNode(method.getValue(), "tags")) {
        final var tag = ApiTool.getNode(method.getValue(), "tags").elements().next().asText();
        mapByTag.put(tag, Map.of(pathItem.getKey(), new ObjectNode(JsonNodeFactory.instance, Map.ofEntries(method))));
      }
    }
    return mapByTag;
  }

  private static MultiValuedMap<String, Map<String, JsonNode>> mapApiGroupsByUrl(final JsonNode openAPI) {
    final var mapByUrl = new ArrayListValuedHashMap<String, Map<String, JsonNode>>();

    for (Iterator<String> it = openAPI.get(PATHS).fieldNames(); it.hasNext();) {
      final var pathUrl = it.next();
      final String[] pathName = pathUrl.split("/");
      mapByUrl.put(pathName[1], Map.of(pathUrl, openAPI.get(PATHS).get(pathUrl)));
    }

    return mapByUrl;
  }

  public static JsonNode getPojoFromSpecFile(final Path baseDir, final SpecFile specFile) {

    return getPojoFromRef(baseDir, specFile.getFilePath());
  }

  public static JsonNode getPojoFromRef(final Path rootFilePath, final String refPath) {
    final JsonNode openAPI;
    try {
      openAPI = PARSER.readTree(readFile(rootFilePath, refPath));
    } catch (final IOException e) {
      throw new FileParseException(refPath, e);
    }

    if (Objects.isNull(openAPI)) {
      throw new FileParseException("empty .yml");
    }

    return openAPI;
  }

  private static String readFile(final Path rootFilePath, final String filePath) throws MalformedURLException {
    URL fileURL = OpenApiUtil.class.getClassLoader().getResource(filePath);
    if (Objects.isNull(fileURL)) {
      final var parentFolder = rootFilePath.resolve(filePath);
      fileURL = parentFolder.toUri().toURL();
    }
    final var sb = new StringBuilder();
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(fileURL.openStream()))) {
      String inputLine;
      while ((inputLine = reader.readLine()) != null) {
        sb.append(inputLine).append(System.lineSeparator());
      }
    } catch (final IOException e) {
      throw new FileParseException("Error reading api file", e);
    }
    return sb.toString();
  }

  public static Map<String, JsonNode> processPaths(final JsonNode openApi, final Map<String, JsonNode> schemaMap) {
    final var basicJsonNodeMap = new HashMap<>(schemaMap);

    for (final var pathElement = openApi.findValue(PATHS).elements(); pathElement.hasNext();) {
      final var pathDefinition = pathElement.next();
      for (Iterator<String> it = pathDefinition.fieldNames(); it.hasNext();) {
        final var pathDefElement = it.next();
        if (REST_VERB_SET.contains(pathDefElement)) {
          processPathContent(basicJsonNodeMap, ApiTool.getNode(pathDefinition, pathDefElement));
        }
      }
    }

    return basicJsonNodeMap;
  }

  private static void processPathContent(final HashMap<String, JsonNode> basicJsonNodeMap, final JsonNode operation) {

    processParameters(basicJsonNodeMap, operation);
    processRequestBody(basicJsonNodeMap, operation);
    processResponses(basicJsonNodeMap, operation);
  }

  private static void processRequestBody(final HashMap<String, JsonNode> basicJsonNodeMap, final JsonNode operation) {
    if (ApiTool.hasNode(operation, "requestBody") && !operation.at("/requestBody/content").isMissingNode()) {
      final var content = operation.at("/requestBody/content");
      final var schema = content.findValue("schema");
      if (!ApiTool.hasRef(schema)) {
        basicJsonNodeMap.put(StringCaseUtils.titleToSnakeCase("InlineObject" + StringUtils.capitalize(getOperationId(operation))), schema);
      } else if (ApiTool.hasItems(schema)) {
        basicJsonNodeMap.put(StringCaseUtils.titleToSnakeCase("InlineObject" + StringUtils.capitalize(ApiTool.getNodeAsString(operation, "operationId"))), ApiTool.getItems(schema));
      }
    }
  }

  private static void processResponses(final HashMap<String, JsonNode> basicJsonNodeMap, final JsonNode operation) {
    if (ApiTool.hasNode(operation, "responses")) {
      final var responses = ApiTool.getNode(operation, "responses");
      for (Iterator<Entry<String, JsonNode>> it = responses.fields(); it.hasNext();) {
        final var response = it.next();
        if (ApiTool.hasContent(response.getValue())) {
          final var schemaList = ApiTool.findContentSchemas(response.getValue());
          for (var schema : schemaList) {
            if (!ApiTool.hasRef(schema) && ApiTool.isObject(schema)) {
              basicJsonNodeMap.put(StringCaseUtils.titleToSnakeCase("InlineResponse" + response.getKey() + StringUtils.capitalize(getOperationId(operation))), schema);
            } else if (ApiTool.isComposed(schema)) {
              basicJsonNodeMap.put(StringCaseUtils.titleToSnakeCase("InlineResponse" + response.getKey() + StringUtils.capitalize(getOperationId(operation)) + getComposedJsonNodeName(schema)), schema);
            }
          }
        }
      }
    }
  }

  private static void processParameters(final HashMap<String, JsonNode> basicJsonNodeMap, final JsonNode operation) {
    if (ApiTool.hasNode(operation, "parameters")) {
      for (Iterator<JsonNode> it = operation.findValue("parameters").elements(); it.hasNext();) {
        final var parameter = it.next();
        if (ApiTool.hasNode(parameter, "content")) {
          basicJsonNodeMap.putIfAbsent(
              StringCaseUtils.titleToSnakeCase("InlineParameter" + StringUtils.capitalize(getOperationId(operation)) + StringUtils.capitalize(ApiTool.getName(parameter))),
              ApiTool.getNode(parameter, "schema"));
        }
      }
    }
  }

  private static String getOperationId(final JsonNode operation) {
    return ApiTool.getNodeAsString(operation, "operationId");
  }

  private static String getComposedJsonNodeName(final JsonNode schema) {
    String composedJsonNodeName = "";
    if (Objects.nonNull(schema.findValue("allOf"))) {
      composedJsonNodeName = "AllOf";
    } else if (Objects.nonNull(schema.findValue("anyOf"))) {
      composedJsonNodeName = "AnyOf";
    } else if (Objects.nonNull(schema.findValue("oneOf"))) {
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


