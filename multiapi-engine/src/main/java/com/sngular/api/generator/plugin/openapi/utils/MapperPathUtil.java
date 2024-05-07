/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.SchemaUtil;
import com.sngular.api.generator.plugin.openapi.exception.DuplicatedOperationException;
import com.sngular.api.generator.plugin.openapi.exception.InvalidOpenAPIException;
import com.sngular.api.generator.plugin.openapi.model.*;
import com.sngular.api.generator.plugin.openapi.model.GlobalObject.GlobalObjectBuilder;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.lang3.StringUtils;

import java.nio.file.Path;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.BiConsumer;

public class MapperPathUtil {

  public static final String INLINE_PARAMETER = "InlineParameter";

  public static final String CONTENT = "content";

  public static final String REQUIRED = "required";

  public static final String REQUEST_BODY = "requestBody";

  public static final String DESCRIPTION = "description";

  public static final String SCHEMA = "schema";

  private MapperPathUtil() {}

  public static GlobalObject mapOpenApiObjectToOurModels(final JsonNode openAPI, final List<AuthSchemaObject> authSchemaList) {
    final var authList = getSecurityRequirementList(ApiTool.getNode(openAPI, "security"), new ArrayList<>());
    final GlobalObjectBuilder globalObject =
        GlobalObject.builder().url(ApiTool.getNode(openAPI, "servers").findValue("url").textValue()).authSchemas(authSchemaList).authentications(authList);
    if (ApiTool.hasNode(openAPI, "components")) {
      globalObject.schemaMap(ApiTool.getComponentSchemas(openAPI));
      globalObject.parameterMap(ApiTool.getParameterSchemas(openAPI));
      globalObject.responseMap(ApiTool.getResponseSchemas(openAPI));
      globalObject.requestBodyMap(ApiTool.getRequestBodySchemas(openAPI));
    } else {
      globalObject.schemaMap(new HashMap<>());
    }
    return globalObject.build();
  }

  private static List<String> getSecurityRequirementList(final JsonNode securityNode, final List<String> authentications) {
    final List<String> authSecList;
    if (Objects.nonNull(securityNode) && !securityNode.isMissingNode()) {
      authSecList = new ArrayList<>();
      securityNode.elements().forEachRemaining(securityRequirement -> authSecList.add(securityRequirement.fieldNames().next()));
    } else {
      authSecList = authentications;
    }
    return authSecList;
  }

  public static List<PathObject> mapPathObjects(final SpecFile specFile, final Collection<Map<String, JsonNode>> path, final GlobalObject globalObject, final Path baseDir) {
    final List<PathObject> pathObjects = new ArrayList<>();
    for (var pathMap : path) {
      for (var pathItem : pathMap.entrySet()) {
        final PathObject pathObject = PathObject.builder()
                                                .pathName(pathItem.getKey())
                                                .globalObject(globalObject)
                                                .operationObjects(mapOperationObject(specFile, pathItem, globalObject, baseDir))
                                                .build();
        pathObjects.add(pathObject);
      }
    }

    return pathObjects;
  }

  private static List<OperationObject> mapOperationObject(final SpecFile specFile, final Entry<String, JsonNode> path, final GlobalObject globalObject, final Path baseDir) {
    final List<OperationObject> operationObjects = new ArrayList<>();
    final List<String> operationIdList = new ArrayList<>();
    final var pathNode = path.getValue();
    final var pathParameters = new ArrayList<ParameterObject>();
    for (Iterator<Entry<String, JsonNode>> it = pathNode.fields(); it.hasNext();) {
      final var field = it.next();
      switch (field.getKey()) {
        case "get":
        case "post":
        case "delete":
        case "put":
        case "patch":
          operationObjects.add(createOperation(field.getValue(), field.getKey().toUpperCase(), specFile, globalObject, operationIdList, baseDir));
          break;
        case "parameters":
          pathParameters.addAll(mapParameterObjects(IteratorUtils.toList(field.getValue().elements()), specFile, null, globalObject, baseDir));
          break;
        default:
          break;
      }
    }
    if (!pathParameters.isEmpty()) {
      final var operationObjectsIt = operationObjects.listIterator();
      while (operationObjectsIt.hasNext()) {
        final var operation = operationObjectsIt.next();
        operationObjectsIt.set(operation.toBuilder().parameterObjects(pathParameters).build());
      }
    }
    return operationObjects;
  }

  private static OperationObject createOperation(
      final JsonNode operation, final String operationType, final SpecFile specFile, final GlobalObject globalObject,
      final List<String> operationIdList, final Path baseDir) {
    Objects.requireNonNull(operation.get("tags"), "Tags element is required");
    return OperationObject.builder()
                          .operationId(mapOperationId(getOperationId(operation), operationIdList))
                          .operationType(operationType)
                          .summary(ApiTool.getNodeAsString(operation, "summary"))
                          .tags(elementsToStrList(operation.get("tags").elements()))
                          .requestObjects(mapRequestObject(specFile, operation, globalObject, baseDir))
                          .responseObjects(mapResponseObject(specFile, globalObject, operation, baseDir))
                          .parameterObjects(mapParameterObjects(IteratorUtils.toList(operation.at("/parameters").elements()), specFile, getOperationId(operation),
                                                                globalObject, baseDir))
                          .securities(getSecurityRequirementList(operation.path("/security"), globalObject.getAuthentications()))
                          .consumes(getRequestList(operation.at("/requestBody")))
                          .produces(getResponseList(operation.at("/responses")))
                          .build();
  }

  private static String getOperationId(final JsonNode operation) {
    return ApiTool.getNodeAsString(operation, "operationId");
  }

  private static List<String> elementsToStrList(final Iterator<JsonNode> tags) {
    final List<String> stringList = new ArrayList<>();
    tags.forEachRemaining(tag -> stringList.add(tag.asText()));
    return stringList;
  }

  private static String mapOperationId(final String operationId, final List<String> operationIdList) {
    if (operationIdList.contains(operationId)) {
      throw new DuplicatedOperationException(operationId);
    } else {
      operationIdList.add(operationId);
      return operationId;
    }

  }

  private static List<String> getRequestList(final JsonNode requestBody) {
    final var consumesList = new ArrayList<String>();
    if (Objects.nonNull(requestBody) && ApiTool.hasNode(requestBody, CONTENT)) {

      final var consumes = ApiTool.getFieldIterator(ApiTool.getNode(requestBody, CONTENT));
      consumes.forEachRemaining(key -> {
        if (!key.getKey().equalsIgnoreCase("*/*")) {
          consumesList.add(key.getKey().replace("\"", "\\\""));
        }
      });
    }

    return consumesList;
  }

  private static List<String> getResponseList(final JsonNode responses) {
    final var producesList = new ArrayList<String>();

    if (Objects.nonNull(responses) && !responses.isEmpty()) {
      responses.elements().forEachRemaining(response -> {
        if (Objects.nonNull(response.findValue(CONTENT))) {
          response.get(CONTENT).fieldNames().forEachRemaining(
              mediaType -> {
              if (!mediaType.equalsIgnoreCase("*/*") && !producesList.contains(mediaType)) {
                producesList.add(mediaType.replace("\"", "\\\""));
              }
            });
        }
      });
    }
    return producesList;
  }

  private static List<RequestObject> mapRequestObject(
      final SpecFile specFile, final JsonNode operation,
      final GlobalObject globalObject, final Path baseDir) {
    final List<RequestObject> requestObjects = new ArrayList<>();
    if (Objects.isNull(getOperationId(operation))) {
      throw new InvalidOpenAPIException();
    }
    final var operationId = getOperationId(operation);
    final String operationIdWithCap = operationId.substring(0, 1).toUpperCase() + operationId.substring(1);
    if (ApiTool.hasNode(operation, REQUEST_BODY)) {
      final var requestBody = ApiTool.getNode(operation, REQUEST_BODY);
      if (!ApiTool.hasRef(requestBody)) {
        requestObjects.add(RequestObject.builder()
                                        .required(ApiTool.hasNode(requestBody, REQUIRED))
                                        .contentObjects(mapContentObject(specFile, ApiTool.getNode(requestBody, CONTENT),
                                                                         "InlineObject" + operationIdWithCap, globalObject, baseDir))
                                        .build());
      } else {
        final var requestBodyNode = globalObject.getRequestBodyNode(MapperUtil.getRefSchemaName(requestBody)).orElseThrow();
        requestObjects.add(RequestObject.builder()
                                        .required(ApiTool.hasNode(requestBody, REQUIRED))
                                        .contentObjects(mapContentObject(specFile, ApiTool.getNode(requestBodyNode, CONTENT),
                                                                         operationIdWithCap, globalObject, baseDir))
                                        .build());
      }
    }
    return requestObjects;
  }

  private static List<ParameterObject> mapParameterObjects(
      final List<JsonNode> parameters, final SpecFile specFile, final String contentClassName,
      final GlobalObject globalObject, final Path baseDir) {
    final List<ParameterObject> parameterObjects = new ArrayList<>();
    if (Objects.nonNull(parameters) && !parameters.isEmpty()) {
      for (JsonNode parameter : parameters) {
        if (ApiTool.hasRef(parameter)) {
          final JsonNode refParameter = globalObject.getParameterNode(MapperUtil.getRefSchemaName(parameter)).orElseThrow();
          parameterObjects.add(buildParameterObject(specFile, globalObject, refParameter, baseDir));
        } else if (ApiTool.hasNode(parameter, CONTENT)) {
          parameterObjects.addAll(buildParameterContent(contentClassName, parameter, specFile, globalObject, baseDir));
        } else {
          parameterObjects.add(buildParameterObject(specFile, globalObject, parameter, baseDir));
        }
      }
    }
    return parameterObjects;
  }

  private static ParameterObject buildParameterObject(
       final SpecFile specFile, final GlobalObject globalObject, final JsonNode refParameter, final Path baseDir) {
    return ParameterObject.builder()
                          .name(ApiTool.getName(refParameter))
                          .required(ApiTool.getNodeAsBoolean(refParameter, REQUIRED))
                          .description(ApiTool.getNodeAsString(refParameter, DESCRIPTION))
                          .in(ApiTool.getNodeAsString(refParameter, "in"))
                          .dataType(getSchemaType(getContentOrSchema(refParameter), TypeConstants.OBJECT, specFile, globalObject, baseDir))
                          .isCollection(ApiTool.hasItems(getContentOrSchema(refParameter)))
                          .build();
  }

  private static JsonNode getContentOrSchema(final JsonNode refParameter) {
    final JsonNode content;
    if (ApiTool.hasNode(refParameter, SCHEMA)) {
      content = ApiTool.getNode(refParameter, SCHEMA);
    } else {
      content = ApiTool.getNode(refParameter, CONTENT);
    }
    return content;
  }

  private static List<ParameterObject> buildParameterContent(
      final String contentClassName, final JsonNode parameter, final SpecFile specFile,
      final GlobalObject globalObject, final Path baseDir) {
    final var content = ApiTool.getNode(parameter, CONTENT);
    final var parameterName = ApiTool.getName(parameter);
    final var parameterObjects = new ArrayList<ParameterObject>();
    for (Iterator<JsonNode> it = content.elements(); it.hasNext();) {
      final var contentType = it.next();
      final String inlineParameter = INLINE_PARAMETER + safeCapitalize(contentClassName)
                                     + StringUtils.capitalize(parameterName);

      final String inlineParameterPojo = getPojoName(inlineParameter, specFile);
      final var builder = ParameterObject.builder()
                                         .name(parameterName)
                                         .required(ApiTool.getNodeAsBoolean(parameter, REQUIRED))
                                         .description(ApiTool.getNodeAsString(parameter, DESCRIPTION))
                                         .in(ApiTool.getNodeAsString(parameter, "in"));
      final var parameterSchema = ApiTool.getNode(contentType, SCHEMA);
      if (TypeConstants.OBJECT.equalsIgnoreCase(ApiTool.getType(parameterSchema))) {
        parameterObjects.add(builder
                               .name(parameterName)
                               .dataType(SchemaFieldObjectType.fromTypeList(inlineParameterPojo))
                               .importName(inlineParameterPojo)
                               .build());
        globalObject.getSchemaMap().put(inlineParameter, parameterSchema);
      } else {
        parameterObjects.add(builder
                               .name(parameterName)
                               .dataType(getSchemaType(parameterSchema, inlineParameterPojo, specFile, globalObject, baseDir))
                               .build());
      }
    }
    return parameterObjects;
  }

  private static String safeCapitalize(final String text) {
    return StringUtils.isEmpty(text) ? "" : StringUtils.capitalize(text);
  }

  private static List<ResponseObject> mapResponseObject(final SpecFile specFile, final GlobalObject globalObject, final JsonNode operation, final Path baseDir) {
    final List<ResponseObject> responseObjects = new ArrayList<>();
    if (ApiTool.hasNode(operation, "responses")) {
      final JsonNode responses = ApiTool.getNode(operation, "responses");
      final var operationId = getOperationId(operation);
      responses
          .fieldNames()
          .forEachRemaining(responseCode ->
                            createResponseObject(specFile, globalObject, responseObjects, operationId, baseDir)
                              .accept(responseCode, ApiTool.getNode(responses, responseCode)));
    }
    return responseObjects;
  }

  @SuppressWarnings("checkstyle:LambdaBodyLength")
  private static BiConsumer<String, JsonNode> createResponseObject(
      final SpecFile specFile, final GlobalObject globalObject,
      final List<ResponseObject> responseObjects, final String operationId, final Path baseDir) {
    return (responseCode, response) ->
      buildResponse(specFile, globalObject, responseObjects, operationId, baseDir, responseCode, response);
  }

  private static void buildResponse(
      final SpecFile specFile, final GlobalObject globalObject, final List<ResponseObject> responseObjects, final String operationId, final Path baseDir, final String responseCode,
      final JsonNode response) {
    var realResponse = response;
    if (ApiTool.hasRef(response)) {
      realResponse = globalObject.getResponseNode(MapperUtil.getRefSchemaName(response)).orElseThrow();
    }
    final String operationIdWithCap = operationId.substring(0, 1).toUpperCase() + operationId.substring(1);
    final var content = ApiTool.getNode(realResponse, CONTENT);
    responseObjects.add(ResponseObject
                          .builder()
                          .responseName(responseCode)
                          .description(StringUtils.defaultIfEmpty(ApiTool.getNodeAsString(realResponse, DESCRIPTION), ""))
                          .contentObjects(mapContentObject(specFile, content, "InlineResponse" + responseCode + operationIdWithCap, globalObject, baseDir))
                          .build());
  }

  private static List<ContentObject> mapContentObject(
      final SpecFile specFile, final JsonNode content, final String inlineObject, final GlobalObject globalObject,
      final Path baseDir) {
    final List<ContentObject> contentObjects = new ArrayList<>();
    if (Objects.nonNull(content)) {
      for (Iterator<String> it = content.fieldNames(); it.hasNext(); ) {
        final String mediaType = it.next();
        final var schema = ApiTool.getNode(ApiTool.getNode(content, mediaType), SCHEMA);
        final String pojoName = preparePojoName(inlineObject, schema, specFile);
        final SchemaFieldObjectType dataType = getSchemaType(schema, pojoName, specFile, globalObject, baseDir);
        final String importName = getImportFromType(dataType);
        SchemaObject schemaObject = null;
        if (mediaType.equals("application/x-www-form-urlencoded") || mediaType.equals("multipart/form-data")) {
          schemaObject = MapperContentUtil.mapComponentToSchemaObject(globalObject.getSchemaMap(), new HashMap<String, SchemaObject>(), schema, dataType.getBaseType(), specFile, baseDir).get("object");
        }
        contentObjects.add(ContentObject.builder()
                .dataType(dataType)
                .name(mediaType)
                .importName(importName)
                .schemaObject(schemaObject)
                .build());
      }
    }
    return contentObjects;
  }

  private static String preparePojoName(final String inlineObject, final JsonNode schema, final SpecFile specFile) {
    final String pojoName;
    if (ApiTool.isAllOf(schema)) {
      pojoName = getPojoName(inlineObject + "AllOf", specFile);
    } else if (ApiTool.isAnyOf(schema)) {
      pojoName = getPojoName(inlineObject + "AnyOf", specFile);
    } else if (ApiTool.isOneOf(schema)) {
      pojoName = getPojoName(inlineObject + "OneOf", specFile);
    } else if (ApiTool.hasRef(schema)) {
      pojoName = getPojoName(inlineObject + MapperUtil.getRefSchemaName(schema), specFile);
    } else {
      pojoName = getPojoName(inlineObject, specFile);
    }

    return pojoName;
  }

  private static SchemaFieldObjectType getSchemaType(
      final JsonNode schema, final String pojoName, final SpecFile specFile, final GlobalObject globalObject,
      final Path baseDir) {
    SchemaFieldObjectType type = null;

    if (ApiTool.hasRef(schema)) {
      final String refSchemaPojoName = MapperUtil.getRef(schema, specFile);
      final JsonNode refSchema = SchemaUtil.solveRef(ApiTool.getRefValue(schema), globalObject.getSchemaMap(),
                                                     baseDir.resolve(specFile.getFilePath()).getParent());
      type = getSchemaType(refSchema, refSchemaPojoName, specFile, globalObject, baseDir);
    } else if (ApiTool.hasAdditionalProperties(schema)) {
      type = getMapSchemaType(schema, pojoName, specFile, globalObject, baseDir);
    } else if (ApiTool.isDateTime(schema)) {
      type = new SchemaFieldObjectType(MapperUtil.getDateType(schema, specFile));
    } else if (ApiTool.hasType(schema)) {
      type = getObjectOrType(schema, pojoName, specFile, globalObject, baseDir);
    } else if (ApiTool.isComposed(schema)) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
    }

    return type;
  }

  private static SchemaFieldObjectType getObjectOrType(
      final JsonNode schema, final String pojoName, final SpecFile specFile, final GlobalObject globalObject,
      final Path baseDir) {
    final SchemaFieldObjectType type;
    switch (ApiTool.getType(schema)) {
      case TypeConstants.OBJECT:
        type = SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
        break;
      case TypeConstants.INTEGER:
        type = new SchemaFieldObjectType(getIntegerFormat(schema));
        break;
      case TypeConstants.NUMBER:
        type = new SchemaFieldObjectType(getNumberFormat(schema));
        break;
      case TypeConstants.BOOLEAN:
        type = new SchemaFieldObjectType(TypeConstants.BOOLEAN);
        break;
      case TypeConstants.ARRAY:
        type = new SchemaFieldObjectType(TypeConstants.ARRAY, getSchemaType(ApiTool.getItems(schema), pojoName, specFile, globalObject, baseDir));
        break;
      default:
        type = new SchemaFieldObjectType(TypeConstants.STRING);
    }
    return type;
  }

  private static SchemaFieldObjectType getMapSchemaType(
      final JsonNode schema, final String pojoName, final SpecFile specFile, final GlobalObject globalObject,
      final Path baseDir) {
    final SchemaFieldObjectType type;

    final JsonNode addPropObj = ApiTool.getAdditionalProperties(schema);
    if (ApiTool.hasProperties(schema)) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
    } else if (TypeConstants.BOOLEAN.equalsIgnoreCase(ApiTool.getType(addPropObj))) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.OBJECT);
    } else {
      type = new SchemaFieldObjectType(TypeConstants.MAP, getSchemaType(addPropObj, pojoName, specFile, globalObject, baseDir));
    }

    return type;
  }

  private static String getIntegerFormat(final JsonNode schema) {
    return TypeConstants.INT_64.equalsIgnoreCase(ApiTool.getFormat(schema)) ? TypeConstants.LONG : TypeConstants.INTEGER;
  }

  private static String getNumberFormat(final JsonNode schema) {
    final String typeName;
    if (TypeConstants.FLOAT.equalsIgnoreCase(ApiTool.getFormat(schema))) {
      typeName = TypeConstants.FLOAT;
    } else if (TypeConstants.DOUBLE.equalsIgnoreCase(ApiTool.getFormat(schema))) {
      typeName = TypeConstants.DOUBLE;
    } else {
      typeName = TypeConstants.INTEGER;
    }

    return typeName;
  }

  private static String getImportFromType(final SchemaFieldObjectType type) {
    SchemaFieldObjectType t = type;
    while (Objects.nonNull(t.getInnerType())) {
      t = t.getInnerType();
    }

    return TypeConstants.ALL_TYPES.contains(t.getBaseType()) ? null : t.getBaseType();
  }

  public static String getPojoName(final String namePojo, final SpecFile specFile) {
    return (StringUtils.isNotBlank(specFile.getModelNamePrefix()) ? specFile.getModelNamePrefix() : "")
           + namePojo
           + (StringUtils.isNotBlank(specFile.getModelNameSuffix()) ? specFile.getModelNameSuffix() : "");
  }

}
