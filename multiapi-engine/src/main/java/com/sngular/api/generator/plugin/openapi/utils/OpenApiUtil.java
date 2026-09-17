/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.net.URI;
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
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import com.sngular.api.generator.plugin.common.tools.SchemaUtil;
import com.sngular.api.generator.plugin.common.tools.StringCaseUtils;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.commons.lang3.StringUtils;

public class OpenApiUtil {

  public static final String PATHS = "paths";

  public static final String WEBHOOKS = "webhooks";

  static final Set<String> REST_VERB_SET = Set.of("get", "post", "delete", "patch", "put");

  private OpenApiUtil() {
  }

  public static MultiValuedMap<String, Map<String, JsonNode>> mapApiGroups(final JsonNode openAPI, final boolean groupByTags) {
    final MultiValuedMap<String, Map<String, JsonNode>> mapApis = new ArrayListValuedHashMap<>();
    final JsonNode pathsNode = openAPI.get(PATHS);
    if (pathsNode instanceof ObjectNode && pathsNode.fields().hasNext()) {
      mapApis.putAll(groupByTags ? mapApiGroupsByTags(pathsNode.fields()) : mapApiGroupsByUrl(openAPI));
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

  private static MultiValuedMap<String, Map<String, JsonNode>> mapApiGroupsByUrl(final JsonNode openAPI) {
    final var mapByUrl = new ArrayListValuedHashMap<String, Map<String, JsonNode>>();

    for (Iterator<String> it = openAPI.get(PATHS).fieldNames(); it.hasNext(); ) {
      final var pathUrl = it.next();
      final String[] pathName = pathUrl.split("/");
      mapByUrl.put(pathName[1], Map.of(pathUrl, openAPI.get(PATHS).get(pathUrl)));
    }

    return mapByUrl;
  }

  private static MultiValuedMap<String, Map<String, JsonNode>> getMapMethodsByTag(final Entry<String, JsonNode> pathItem) {
    final MultiValuedMap<String, Map<String, JsonNode>> mapByTag = new ArrayListValuedHashMap<>();
    final var operations = IteratorUtils.filteredIterator(pathItem.getValue().fields(), opProperty -> REST_VERB_SET.contains(opProperty.getKey()));
    while (operations.hasNext()) {
      final var method = operations.next();
      if (ApiTool.hasNode(method.getValue(), "tags")) {
        final var tag = ApiTool.getNode(method.getValue(), "tags").elements().next().asText();
        mapByTag.put(tag, Map.of(pathItem.getKey(), buildTaggedPathItem(pathItem.getValue(), method)));
      }
    }
    return mapByTag;
  }

  private static ObjectNode buildTaggedPathItem(final JsonNode pathItem, final Entry<String, JsonNode> method) {
    final var taggedPathItem = JsonNodeFactory.instance.objectNode();
    taggedPathItem.set(method.getKey(), method.getValue());
    pathItem.fields().forEachRemaining(field -> {
      if (!REST_VERB_SET.contains(field.getKey()) && !taggedPathItem.has(field.getKey())) {
        taggedPathItem.set(field.getKey(), field.getValue());
      }
    });
    return taggedPathItem;
  }

  public static JsonNode getPojoFromSpecFile(final Path baseDir, final SpecFile specFile) {

    return SchemaUtil.getPojoFromRef(baseDir.toUri(), specFile.getFilePath());
  }

  /**
   * Merges the OpenAPI 3.1 top-level {@code webhooks} object into {@code paths} so the existing
   * path pipeline generates a handler interface and the request/response payload models for each
   * webhook. Each webhook is a Path Item Object keyed by name; it is added under a {@code "/"}-
   * prefixed key (webhooks have no URL) so the by-url grouping treats the webhook name as the
   * endpoint. Existing {@code paths} entries take precedence and are never overwritten.
   *
   * @param openApi the parsed root contract; its {@code paths} node is created/extended in place.
   */
  public static void mergeWebhooksIntoPaths(final JsonNode openApi) {
    final JsonNode webhooks = openApi.get(WEBHOOKS);
    if (webhooks instanceof ObjectNode && openApi instanceof ObjectNode) {
      final ObjectNode root = (ObjectNode) openApi;
      final ObjectNode paths = root.has(PATHS) && root.get(PATHS).isObject()
          ? (ObjectNode) root.get(PATHS)
          : root.putObject(PATHS);
      webhooks.fields().forEachRemaining(webhook -> {
        final String webhookName = webhook.getKey();
        final String pathKey = webhookName.startsWith("/") ? webhookName : "/" + webhookName;
        // A leading-slash-only key would break the by-url grouping (pathUrl.split("/")[1]).
        if (StringUtils.isNotBlank(StringUtils.strip(webhookName, "/")) && !paths.has(pathKey)) {
          defaultOperationTags(webhook.getValue(), StringUtils.strip(webhookName, "/"));
          paths.set(pathKey, webhook.getValue());
        }
      });
    }
  }

  /**
   * Ensures every operation of a webhook-derived Path Item carries a {@code tags} entry. Webhook
   * operations normally omit {@code tags}, but the path pipeline requires one; a missing/empty
   * {@code tags} is defaulted to the webhook name so generation works in both grouping modes.
   */
  private static void defaultOperationTags(final JsonNode pathItem, final String defaultTag) {
    if (pathItem instanceof ObjectNode) {
      pathItem.fields().forEachRemaining(field -> {
        if (REST_VERB_SET.contains(field.getKey()) && field.getValue() instanceof ObjectNode) {
          final ObjectNode operation = (ObjectNode) field.getValue();
          if (!ApiTool.hasNode(operation, "tags") || !operation.get("tags").isArray() || operation.get("tags").isEmpty()) {
            operation.putArray("tags").add(defaultTag);
          }
        }
      });
    }
  }

  /**
   * Dereferences Path Item Objects that are declared as a {@code $ref} to another file (modular
   * contracts). These references are otherwise never resolved, so the affected paths silently
   * disappear from generation. The referenced Path Item is resolved and set in place, so every
   * downstream consumer (API grouping, path mapping and model extraction) sees the real operations.
   *
   * @param openApi      the parsed root contract; its {@code paths} node is mutated in place.
   * @param rootFilePath base URI used to resolve relative external references.
   */
  public static void solvePathRefs(final JsonNode openApi, final URI rootFilePath) {
    final JsonNode pathsNode = openApi.get(PATHS);
    if (pathsNode instanceof ObjectNode) {
      final ObjectNode paths = (ObjectNode) pathsNode;
      final Map<String, JsonNode> resolvedItems = new HashMap<>();
      paths.fields().forEachRemaining(pathItem -> {
        final JsonNode pathValue = pathItem.getValue();
        if (ApiTool.hasRef(pathValue)) {
          final JsonNode resolved = SchemaUtil.solveRef(ApiTool.getRefValue(pathValue), new HashMap<>(), rootFilePath);
          if (Objects.nonNull(resolved)) {
            resolvedItems.put(pathItem.getKey(), resolved);
          }
        }
      });
      resolvedItems.forEach(paths::set);
    }
  }

  public static Map<String, JsonNode> processPaths(final JsonNode openApi, final Map<String, JsonNode> schemaMap, SpecFile specFile) {
    final JsonNode pathsNode = openApi.get(PATHS);
    if (pathsNode == null) {
      return schemaMap;
    }
    for (final var pathElement = pathsNode.elements(); pathElement.hasNext(); ) {
      final var pathDefinition = pathElement.next();
      for (Iterator<String> it = pathDefinition.fieldNames(); it.hasNext(); ) {
        final var pathDefElement = it.next();
        if (REST_VERB_SET.contains(pathDefElement)) {
          processPathContent(schemaMap, ApiTool.getNode(pathDefinition, pathDefElement), specFile);
        }
      }
    }

    return schemaMap;
  }

  private static void processPathContent(final Map<String, JsonNode> basicJsonNodeMap, final JsonNode operation, SpecFile specFile) {

    processParameters(basicJsonNodeMap, operation, specFile);
    processRequestBody(basicJsonNodeMap, operation, specFile);
    processResponses(basicJsonNodeMap, operation, specFile);
  }

  private static void processParameters(final Map<String, JsonNode> basicJsonNodeMap, final JsonNode operation, SpecFile specFile) {
    if (ApiTool.hasNode(operation, "parameters")) {
      for (Iterator<JsonNode> it = operation.findValue("parameters").elements(); it.hasNext(); ) {
        final var parameter = it.next();
        if (ApiTool.hasNode(parameter, "content")) {
          basicJsonNodeMap.putIfAbsent(
              StringCaseUtils.titleToSnakeCase(
                  MapperUtil.getPojoName("InlineParameter" + StringUtils.capitalize(getOperationId(operation)) + StringUtils.capitalize(ApiTool.getName(parameter)), specFile)),
              ApiTool.getNode(parameter, "schema"));
        }
      }
    }
  }

  private static void processRequestBody(final Map<String, JsonNode> basicJsonNodeMap, final JsonNode operation, SpecFile specFile) {
    if (ApiTool.hasNode(operation, "requestBody") && !operation.at("/requestBody/content").isMissingNode()) {
      final var content = operation.at("/requestBody/content");
      if (content.has("multipart/form-data")) {
        // multipart parts are exposed individually as @RequestPart parameters,
        // so no wrapper model is generated for them.
        return;
      }
      final var schema = content.findValue("schema");
      if (!ApiTool.hasRef(schema)) {
        basicJsonNodeMap.put(StringCaseUtils.titleToSnakeCase(MapperUtil.getPojoName("InlineObject" + StringUtils.capitalize(getOperationId(operation)), specFile)), schema);
      } else if (ApiTool.hasItems(schema)) {
        basicJsonNodeMap.put(
            StringCaseUtils.titleToSnakeCase(MapperUtil.getPojoName("InlineObject" + StringUtils.capitalize(ApiTool.getNodeAsString(operation, "operationId")), specFile)),
            ApiTool.getItems(schema));
      }
    }
  }

  private static void processResponses(final Map<String, JsonNode> basicJsonNodeMap, final JsonNode operation, SpecFile specFile) {
    if (ApiTool.hasNode(operation, "responses")) {
      final var responses = ApiTool.getNode(operation, "responses");
      for (Iterator<Entry<String, JsonNode>> it = responses.fields(); it.hasNext(); ) {
        final var response = it.next();
        if (ApiTool.hasContent(response.getValue())) {
          final var schemaList = ApiTool.findContentSchemas(response.getValue());
          for (var schema : schemaList) {
            if (!ApiTool.hasRef(schema) && ApiTool.isObject(schema)) {
              basicJsonNodeMap.put(
                  StringCaseUtils.titleToSnakeCase(MapperUtil.getPojoName("InlineResponse" + response.getKey() + StringUtils.capitalize(getOperationId(operation)), specFile)),
                  schema);
            } else if (ApiTool.isComposed(schema)) {
              basicJsonNodeMap.put(StringCaseUtils.titleToSnakeCase(
                                       MapperUtil.getPojoName("InlineResponse" + response.getKey() + StringUtils.capitalize(getOperationId(operation)) + getComposedJsonNodeName(schema), specFile)),
                                   schema);
            }
          }
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


