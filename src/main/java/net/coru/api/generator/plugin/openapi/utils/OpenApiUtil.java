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
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import io.swagger.v3.parser.exception.ReadContentException;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
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
        if (!mapApis.containsKey(mapPathItems.getKey())) {
          mapApis.put(mapPathItems.getKey(), new HashMap<>());
        }
        mapApis.get(mapPathItems.getKey()).put(openAPIGetPathsEntry.getKey(), mapPathItems.getValue());
      }
    }

    return mapApis;

  }

  private static HashMap<String, PathItem> getMapPathItemsByTag(final PathItem pathItem) {
    final var mapByTag = new HashMap<String, PathItem>();

    for (Entry<HttpMethod, Operation> operation : pathItem.readOperationsMap().entrySet()) {
      if (null != operation.getValue().getTags() && !operation.getValue().getTags().isEmpty()) {
        final var pathItemClone = pathItemOperationsClear(pathItem);
        if (!mapByTag.containsKey(operation.getValue().getTags().get(0))) {
          mapByTag.put(operation.getValue().getTags().get(0), pathItemClone);
        }
        mapByTag.get(operation.getValue().getTags().get(0)).operation(operation.getKey(), operation.getValue());
      }
    }
    return mapByTag;
  }

  private static PathItem pathItemOperationsClear(final PathItem pathItem) {
    final PathItem pathItemClone;
    pathItemClone = pathItem;
    pathItemClone.operation(HttpMethod.GET, null);
    pathItemClone.operation(HttpMethod.POST, null);
    pathItemClone.operation(HttpMethod.DELETE, null);
    pathItemClone.operation(HttpMethod.PATCH, null);
    pathItemClone.operation(HttpMethod.HEAD, null);
    pathItemClone.operation(HttpMethod.OPTIONS, null);
    pathItemClone.operation(HttpMethod.TRACE, null);
    pathItemClone.operation(HttpMethod.PUT, null);

    return pathItemClone;
  }

  private static HashMap<String, HashMap<String, PathItem>> mapApiGroupsByUrl(final OpenAPI openAPI) {
    final var mapByUrl = new HashMap<String, HashMap<String, PathItem>>();

    for (Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
      final String[] pathName = openAPIGetPathsEntry.getKey().split("/");
      if (!mapByUrl.containsKey(pathName[1])) {
        mapByUrl.put(pathName[1], new HashMap<>());
      }
      mapByUrl.get(pathName[1]).put(openAPIGetPathsEntry.getKey(), openAPIGetPathsEntry.getValue());
    }

    return mapByUrl;
  }

  public static OpenAPI getPojoFromSwagger(final FileSpec fileSpec) throws MojoExecutionException {
    final OpenAPI openAPI;
    try {
      final SwaggerParseResult result = new OpenAPIParser().readLocation(fileSpec.getInputSpec(), null, null);
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

    if (null != components.getSchemas() && !components.getSchemas().isEmpty()) {

      components.getSchemas().forEach((key, value) -> {
        if (value.getType().equals("object")) {
          listObject.add(key);
        }
      });
    }
    return listObject;
  }

  public static Map<String, Schema> processBasicSchemas(final OpenAPI openApi) {
    final var basicSchemaMap = new HashMap<String, Schema>();

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

  private static void processContentForBasicSchemas(final HashMap<String, Schema> basicSchemaMap, final Operation operation) {

    processOperationRequestBody(basicSchemaMap, operation);
    /*if (Objects.nonNull(operation.getRequestBody()) && Objects.nonNull(operation.getRequestBody().getContent())) {
      final String operationIdWithCap = operation.getOperationId().substring(0, 1).toUpperCase() + operation.getOperationId().substring(1);
      operation.getRequestBody().getContent().forEach((key, value) -> {
        if (value.getSchema().get$ref() == null || (Objects.nonNull(value.getSchema().getItems()) && value.getSchema().getItems().get$ref() == null)) {
          basicSchemaMap.put("InlineObject" + operationIdWithCap,
                             value.getSchema());
        }
      });
    }*/
    processOperationResponses(basicSchemaMap, operation);
    /*for (Entry<String, ApiResponse> response : operation.getResponses().entrySet()) {
      if (Objects.nonNull(response.getValue().getContent())) {
        response.getValue().getContent().forEach((key, value) -> {
          if (value.getSchema().get$ref() == null || (Objects.nonNull(value.getSchema().getItems()) && value.getSchema().getItems().get$ref() == null)) {
            final String operationIdWithCap = operation.getOperationId().substring(0, 1).toUpperCase() + operation.getOperationId().substring(1);
            basicSchemaMap.put("InlineResponse" + response.getKey() + operationIdWithCap,
                               value.getSchema());
          }
        });
      }
    }*/
  }

  private static void processOperationRequestBody(final HashMap<String, Schema> basicSchemaMap, final Operation operation) {
    if (Objects.nonNull(operation.getRequestBody()) && Objects.nonNull(operation.getRequestBody().getContent())) {
      final String operationIdWithCap = operation.getOperationId().substring(0, 1).toUpperCase() + operation.getOperationId().substring(1);
      operation.getRequestBody().getContent().forEach((key, value) -> {
        if (value.getSchema().get$ref() == null || Objects.nonNull(value.getSchema().getItems()) && value.getSchema().getItems().get$ref() == null) {
          basicSchemaMap.put("InlineObject" + operationIdWithCap,
                             value.getSchema());
        }
      });
    }
  }

  private static void processOperationResponses(final HashMap<String, Schema> basicSchemaMap, final Operation operation) {
    for (Entry<String, ApiResponse> response : operation.getResponses().entrySet()) {
      if (Objects.nonNull(response.getValue().getContent())) {
        response.getValue().getContent().forEach((key, value) -> {
          if (value.getSchema().get$ref() == null || Objects.nonNull(value.getSchema().getItems()) && value.getSchema().getItems().get$ref() == null) {
            final String operationIdWithCap = operation.getOperationId().substring(0, 1).toUpperCase() + operation.getOperationId().substring(1);
            basicSchemaMap.put("InlineResponse" + response.getKey() + operationIdWithCap,
                               value.getSchema());
          }
        });
      }
    }
  }
}


