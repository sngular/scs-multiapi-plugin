/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
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
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import io.swagger.v3.parser.exception.ReadContentException;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.MojoExecutionException;

public class OpenApiUtil {

  private OpenApiUtil() {}

  public static Map<String, HashMap<String, PathItem>> mapApiGroups(final OpenAPI openAPI, final boolean groupByTags) {
    var mapApis = new HashMap<String, HashMap<String, PathItem>>();

    if (!openAPI.getPaths().isEmpty()) {
      mapApis = groupByTags ? mapApiGroupsByTags(openAPI) : mapApiGroupsByUrl(openAPI);
    }

    return mapApis;
  }

  private static HashMap<String, HashMap<String, PathItem>> mapApiGroupsByTags(final OpenAPI openAPI) {

    final var mapApis = new HashMap<String, HashMap<String, PathItem>>();
    for (Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
      final var mapPathItemsByTag = getMapPathItemsByTag(openAPIGetPathsEntry.getValue());
      for (Entry<String, PathItem> mapPathItems : mapPathItemsByTag.entries()) {
        mapApis.compute(mapPathItems.getKey(), (key, value) -> initOrInsert(openAPIGetPathsEntry, mapPathItems, value));
      }
    }

    return mapApis;

  }

  private static HashMap<String, PathItem> initOrInsert(
      final Entry<String, PathItem> openAPIGetPathsEntry, final Entry<String, PathItem> mapPathItems,
      final HashMap<String, PathItem> value) {
    var newValue = value;
    if (Objects.isNull(newValue)) {
      newValue = new HashMap<>();
    }
    newValue.put(openAPIGetPathsEntry.getKey(), mapPathItems.getValue());

    return newValue;
  }

  private static MultiValuedMap<String, PathItem> getMapPathItemsByTag(final PathItem pathItem) {
    MultiValuedMap<String, PathItem> mapByTag = new ArrayListValuedHashMap<>();

    for (Entry<HttpMethod, Operation> operation : pathItem.readOperationsMap().entrySet()) {
      if (CollectionUtils.isNotEmpty(operation.getValue().getTags())) {
        final var tag = operation.getValue().getTags().get(0);
        mapByTag.put(tag, pathItem);
      }
    }
    return mapByTag;
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

    if (Objects.isNull(openAPI)) {
      throw new MojoExecutionException("Code generation failed why .yaml is empty");
    }

    return openAPI;
  }

  public static List<String> getListComponentsObjects(final OpenAPI openAPI) {
    final Components components = openAPI.getComponents();
    final var listObject = new ArrayList<String>();

    if (Objects.nonNull(components)) {
      components.getSchemas().forEach((key, value) -> {
        if ("object".equals(value.getType()) || MapperPathUtil.checkSchemaCombinator(value)) {
          listObject.add(key);
        }
      });
    }
    return listObject;
  }

  public static Map<String, Schema<?>> processBasicSchemas(final OpenAPI openApi) {
    final var basicSchemaMap = new HashMap<String, Schema<?>>();

    for (Entry<String, PathItem> pathItem : openApi.getPaths().entrySet()) {
      final PathItem path = pathItem.getValue();
      if (Objects.nonNull(path.getGet())) {
        processContentForBasicSchemas(basicSchemaMap, path.getGet());
      }
      if (Objects.nonNull(path.getPost())) {
        processContentForBasicSchemas(basicSchemaMap, path.getPost());
      }
      if (Objects.nonNull(path.getPut())) {
        processContentForBasicSchemas(basicSchemaMap, path.getPut());
      }
      if (Objects.nonNull(path.getDelete())) {
        processContentForBasicSchemas(basicSchemaMap, path.getDelete());
      }
      if (Objects.nonNull(path.getPatch())) {
        processContentForBasicSchemas(basicSchemaMap, path.getPatch());
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
        if (Objects.isNull(value.getSchema().get$ref())) {
          basicSchemaMap.put("InlineObject" + StringUtils.capitalize(operation.getOperationId()),
                             value.getSchema());
        } else if (Objects.nonNull(value.getSchema().getItems())) {
          basicSchemaMap.put("InlineObject" + StringUtils.capitalize(operation.getOperationId()),
                             value.getSchema().getItems());
        }
      });
    }
  }

  private static void processOperationResponses(final HashMap<String, Schema<?>> basicSchemaMap, final Operation operation) {
    for (Entry<String, ApiResponse> response : operation.getResponses().entrySet()) {
      if (Objects.nonNull(response.getValue().getContent())) {
        response.getValue().getContent().forEach((key, value) -> {
          if (Objects.isNull(value.getSchema().get$ref()) && "object".equalsIgnoreCase(value.getSchema().getType())) {
            basicSchemaMap.put("InlineResponse" + response.getKey() + StringUtils.capitalize(operation.getOperationId()),
                               value.getSchema());
          }
        });
      }
      if (Objects.nonNull(operation.getParameters())) {
        for (Parameter parameter : operation.getParameters()) {
          if (Objects.nonNull(parameter.getContent())) {
            parameter.getContent()
                     .forEach((name, mediaType) -> basicSchemaMap.putIfAbsent(
                         "InlineParameter" + StringUtils.capitalize(operation.getOperationId()) + StringUtils.capitalize(parameter.getName()),
                         mediaType.getSchema()));
          }
        }
      }
    }
  }

  public static String processJavaFileName(final String apisEntry) {
    String javaFileName = "";
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


