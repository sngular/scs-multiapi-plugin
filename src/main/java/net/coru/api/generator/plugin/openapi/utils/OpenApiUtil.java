/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import io.swagger.v3.parser.exception.ReadContentException;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.MojoExecutionException;

public class OpenApiUtil {

  private OpenApiUtil() {}

  public static Map<String, HashMap<String, PathItem>> mapApiGroups(final OpenAPI openAPI, final Boolean groupByTags) {
    var mapApis = new HashMap<String, HashMap<String, PathItem>>();

    if (!openAPI.getPaths().isEmpty()) {
      mapApis = null != groupByTags && groupByTags ? mapApiGroupsByTags(openAPI) : mapApiGroupsByUrl(openAPI);
    }

    return mapApis;
  }

  private static HashMap<String, HashMap<String, PathItem>> mapApiGroupsByTags(final OpenAPI openAPI) {

    final var mapApis = new HashMap<String, HashMap<String, PathItem>>();
    for (Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
      final var mapPathItemsByTag = getMapPathItemsByTag(openAPIGetPathsEntry.getValue());
      for (Entry<String, PathItem> mapPathItems : mapPathItemsByTag.entrySet()) {
        mapApis.compute(mapPathItems.getKey(), (key, value) -> initOrInsert(openAPIGetPathsEntry, mapPathItems, value));
      }
    }

    return mapApis;

  }

  private static HashMap<String, PathItem> initOrInsert(final Entry<String, PathItem> openAPIGetPathsEntry, final Entry<String, PathItem> mapPathItems,
      final HashMap<String, PathItem> value) {
    var newValue = value;
    if (Objects.isNull(newValue)) {
      newValue = new HashMap<>();
    }
    newValue.put(openAPIGetPathsEntry.getKey(), mapPathItems.getValue());

    return newValue;
  }

  private static HashMap<String, PathItem> getMapPathItemsByTag(final PathItem pathItem) {
    final var mapByTag = new HashMap<String, PathItem>();

    for (Entry<HttpMethod, Operation> operation : pathItem.readOperationsMap().entrySet()) {
      if (CollectionUtils.isNotEmpty(operation.getValue().getTags())) {
        final var pathItemClone = pathItemOperationsClear(pathItem);
        final var tag = operation.getValue().getTags().get(0);
        mapByTag.putIfAbsent(tag, pathItemClone);
        mapByTag.get(tag).operation(operation.getKey(), operation.getValue());
      }
    }
    return mapByTag;
  }

  private static PathItem pathItemOperationsClear(final PathItem pathItem) {
    pathItem.operation(HttpMethod.GET, null);
    pathItem.operation(HttpMethod.POST, null);
    pathItem.operation(HttpMethod.DELETE, null);
    pathItem.operation(HttpMethod.PATCH, null);
    pathItem.operation(HttpMethod.HEAD, null);
    pathItem.operation(HttpMethod.OPTIONS, null);
    pathItem.operation(HttpMethod.TRACE, null);
    pathItem.operation(HttpMethod.PUT, null);

    return pathItem;
  }

  private static HashMap<String, HashMap<String, PathItem>> mapApiGroupsByUrl(final OpenAPI openAPI) {
    final var mapByUrl = new HashMap<String, HashMap<String, PathItem>>();

    for (Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
      final String[] pathName = openAPIGetPathsEntry.getKey().split("/");
      mapByUrl.putIfAbsent(pathName[1], new HashMap<>());
      mapByUrl.get(pathName[1]).put(openAPIGetPathsEntry.getKey(), openAPIGetPathsEntry.getValue());
    }

    return mapByUrl;
  }

  public static OpenAPI getPojoFromSwagger(final FileSpec fileSpec) throws MojoExecutionException {
    final OpenAPI openAPI;
    final ParseOptions options = new ParseOptions();
    options.setResolve(true);
    try {
      final SwaggerParseResult result = new OpenAPIParser().readLocation(fileSpec.getFilePath(), null, options);
      openAPI = result.getOpenAPI();
    } catch (final ReadContentException e) {
      throw new MojoExecutionException("Code generation failed when parser the .yaml file ");
    }

    if (openAPI == null) {
      throw new MojoExecutionException("Code generation failed why .yaml is empty");
    }

    return openAPI;
  }

  public static List<String> getListComponentsObjects(final OpenAPI openAPI) {
    final Components components = openAPI.getComponents();
    final var listObject = new ArrayList<String>();

    if (MapUtils.isNotEmpty(components.getSchemas())) {
      components.getSchemas().forEach((key, value) -> {
        if (value.getType().equals("object")) {
          listObject.add(key);
        }
      });
    }
    return listObject;
  }

  public static Map<String, Schema<?>> processBasicSchemas(final OpenAPI openApi) {
    final var basicSchemaMap = new HashMap<String, Schema<?>>();

    for (Entry<String, PathItem> pathItem : openApi.getPaths().entrySet()) {
      if (Objects.nonNull(pathItem.getValue().getGet())) {
        processContentForBasicSchemas(basicSchemaMap, pathItem.getValue().getGet());
      }
      if (Objects.nonNull(pathItem.getValue().getPost())) {
        processContentForBasicSchemas(basicSchemaMap, pathItem.getValue().getPost());
      }
      if (Objects.nonNull(pathItem.getValue().getPut())) {
        processContentForBasicSchemas(basicSchemaMap, pathItem.getValue().getPut());
      }
      if (Objects.nonNull(pathItem.getValue().getDelete())) {
        processContentForBasicSchemas(basicSchemaMap, pathItem.getValue().getDelete());
      }
      if (Objects.nonNull(pathItem.getValue().getPatch())) {
        processContentForBasicSchemas(basicSchemaMap, pathItem.getValue().getPatch());
      }
    }

    return basicSchemaMap;
  }

  private static void processContentForBasicSchemas(final HashMap<String, Schema<?>> basicSchemaMap, final Operation operation) {

    processOperationRequestBody(basicSchemaMap, operation);
    processOperationResponses(basicSchemaMap, operation);
  }

  private static void processOperationRequestBody(final HashMap<String, Schema<?>> basicSchemaMap, final Operation operation) {
    if (Objects.nonNull(operation.getRequestBody()) && Objects.nonNull(operation.getRequestBody().getContent())) {
      operation.getRequestBody().getContent().forEach((key, value) -> {
        if (value.getSchema().get$ref() == null || Objects.nonNull(value.getSchema().getItems()) && value.getSchema().getItems().get$ref() == null) {
          basicSchemaMap.put("InlineObject" + StringUtils.capitalize(operation.getOperationId()),
                             value.getSchema());
        }
      });
    }
  }

  private static void processOperationResponses(final HashMap<String, Schema<?>> basicSchemaMap, final Operation operation) {
    for (Entry<String, ApiResponse> response : operation.getResponses().entrySet()) {
      if (Objects.nonNull(response.getValue().getContent())) {
        response.getValue().getContent().forEach((key, value) -> {
          if (value.getSchema().get$ref() == null || Objects.nonNull(value.getSchema().getItems()) && value.getSchema().getItems().get$ref() == null) {
            basicSchemaMap.put("InlineResponse" + response.getKey() + StringUtils.capitalize(operation.getOperationId()),
                               value.getSchema());
          }
        });
      }
    }
  }
}


