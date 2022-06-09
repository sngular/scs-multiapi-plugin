/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Objects;

import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import org.apache.maven.plugin.MojoExecutionException;

public class OpenApiUtil {

  public static HashMap<String, HashMap<String, PathItem>> mapApiGroups(OpenAPI openAPI, Boolean groupByTags) {
    var mapApis = new HashMap<String, HashMap<String, PathItem>>();

    if (!openAPI.getPaths().isEmpty()) {
      mapApis = null != groupByTags && groupByTags ? mapApiGroupsByTags(openAPI) : mapApiGroupsByUrl(openAPI);
    }

    return mapApis;
  }

  private static HashMap<String, HashMap<String, PathItem>> mapApiGroupsByTags(OpenAPI openAPI) {

    var mapApis = new HashMap<String, HashMap<String, PathItem>>();
    for (Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
      var mapPathItemsByTag = getMapPathItemsByTag(openAPIGetPathsEntry.getValue());
      for (Entry<String, PathItem> mapPathItems : mapPathItemsByTag.entrySet()) {
        if (!mapApis.containsKey(mapPathItems.getKey())) {
          mapApis.put(mapPathItems.getKey(), new HashMap<>());
        }
        mapApis.get(mapPathItems.getKey()).put(openAPIGetPathsEntry.getKey(), mapPathItems.getValue());
      }
    }

    return mapApis;

  }

  private static HashMap<String, PathItem> getMapPathItemsByTag(PathItem pathItem) {
    var mapByTag = new HashMap<String, PathItem>();

    for (Entry<HttpMethod, Operation> operation : pathItem.readOperationsMap().entrySet()) {
      if (null != operation.getValue().getTags() && !operation.getValue().getTags().isEmpty()) {
        var pathItemClone = pathItemOperationsClear(pathItem);
        if (!mapByTag.containsKey(operation.getValue().getTags().get(0))) {
          mapByTag.put(operation.getValue().getTags().get(0), pathItemClone);
        }
        mapByTag.get(operation.getValue().getTags().get(0)).operation(operation.getKey(), operation.getValue());
      }
    }
    return mapByTag;
  }

  private static PathItem pathItemOperationsClear(PathItem pathItem) {
    var pathItemClone = new PathItem();
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

  private static HashMap<String, HashMap<String, PathItem>> mapApiGroupsByUrl(OpenAPI openAPI) {
    var mapByUrl = new HashMap<String, HashMap<String, PathItem>>();

    for (Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
      String[] pathName = openAPIGetPathsEntry.getKey().split("/");
      if (!mapByUrl.containsKey(pathName[1])) {
        mapByUrl.put(pathName[1], new HashMap<>());
      }
      mapByUrl.get(pathName[1]).put(openAPIGetPathsEntry.getKey(), openAPIGetPathsEntry.getValue());
    }

    return mapByUrl;
  }

  public static OpenAPI getPojoFromSwagger(FileSpec fileSpec) throws MojoExecutionException {
    OpenAPI openAPI;
    try {
      SwaggerParseResult result = new OpenAPIParser().readLocation(fileSpec.getInputSpec(), null, null);
      openAPI = result.getOpenAPI();

    } catch (Exception e) {
      throw new MojoExecutionException("Code generation failed when parser the .yaml file ");
    }

    if (openAPI == null) {
      throw new MojoExecutionException("Code generation failed why .yaml is empty");
    }

    return openAPI;
  }

  public static List<String> getListComponentsObjects(OpenAPI openAPI) {
    Components components = openAPI.getComponents();
    var listObject = new ArrayList<String>();

    if (null == components.getSchemas() || components.getSchemas().isEmpty()) {
      return listObject;
    }
    components.getSchemas().forEach((key, value) -> {
      if (value.getType().equals("object")) {
        listObject.add(key);
      }
    });

    return listObject;
  }

  public static HashMap<String, Schema> processBasicSchemas(OpenAPI openApi) {
    var basicSchemaMap = new HashMap<String, Schema>();

    for (Entry<String, PathItem> pathItem : openApi.getPaths().entrySet()) {
      if (Objects.nonNull(pathItem.getValue().getGet())) {
        processContentSchema(basicSchemaMap, pathItem.getValue().getGet());
      }
      if (Objects.nonNull(pathItem.getValue().getPost())) {
        processContentSchema(basicSchemaMap, pathItem.getValue().getPost());
      }
      if (Objects.nonNull(pathItem.getValue().getPut())) {
        processContentSchema(basicSchemaMap, pathItem.getValue().getPost());
      }
      if (Objects.nonNull(pathItem.getValue().getDelete())) {
        processContentSchema(basicSchemaMap, pathItem.getValue().getPost());
      }
      if (Objects.nonNull(pathItem.getValue().getPatch())) {
        processContentSchema(basicSchemaMap, pathItem.getValue().getPatch());
      }
    }

    return basicSchemaMap;
  }

  private static void processContentSchema(HashMap<String, Schema> basicSchemaMap, Operation operation) {

    if (Objects.nonNull(operation.getRequestBody()) && Objects.nonNull(operation.getRequestBody().getContent())) {

      String firstLetter = operation.getOperationId().substring(0, 1);
      String remainingLetters = operation.getOperationId().substring(1);

      String operationId = firstLetter.toUpperCase() + remainingLetters;
      operation.getRequestBody().getContent().forEach((key, value) -> {
        if (value.getSchema().get$ref() == null || (Objects.nonNull(value.getSchema().getItems()) && value.getSchema().getItems().get$ref() == null)) {
          basicSchemaMap.put("InlineObject" + operationId,
                             value.getSchema());
        }
      });
    }
    for (Entry<String, ApiResponse> response : operation.getResponses().entrySet()) {
      if (Objects.nonNull(response.getValue().getContent())) {
        response.getValue().getContent().forEach((key, value) -> {
          if (value.getSchema().get$ref() == null || (Objects.nonNull(value.getSchema().getItems()) && value.getSchema().getItems().get$ref() == null)) {
            basicSchemaMap.put("InlineResponse" + response.getKey(),
                               value.getSchema());
          }
        });
      }
    }
  }
}


