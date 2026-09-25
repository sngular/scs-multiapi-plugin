/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.net.URI;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.MapperContentUtil;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import com.sngular.api.generator.plugin.common.tools.SchemaUtil;
import com.sngular.api.generator.plugin.common.tools.StringCaseUtils;
import com.sngular.api.generator.plugin.openapi.exception.DuplicatedOperationException;
import com.sngular.api.generator.plugin.openapi.exception.InvalidOpenAPIException;
import com.sngular.api.generator.plugin.openapi.model.AuthSchemaObject;
import com.sngular.api.generator.plugin.openapi.model.ContentObject;
import com.sngular.api.generator.plugin.openapi.model.GlobalObject;
import com.sngular.api.generator.plugin.openapi.model.GlobalObject.GlobalObjectBuilder;
import com.sngular.api.generator.plugin.openapi.model.OperationObject;
import com.sngular.api.generator.plugin.openapi.model.ParameterObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import com.sngular.api.generator.plugin.openapi.model.RequestObject;
import com.sngular.api.generator.plugin.openapi.model.ResponseObject;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperPathUtil {

  public static final String INLINE_PARAMETER = "InlineParameter";

  public static final String CONTENT = "content";

  public static final String REQUIRED = "required";

  public static final String REQUEST_BODY = "requestBody";

  public static final String DESCRIPTION = "description";

  public static final String SCHEMA = "schema";

  private MapperPathUtil() {
  }

  public static GlobalObject mapOpenApiObjectToOurModels(final JsonNode openAPI, final List<AuthSchemaObject> authSchemaList) {
    final var authList = getSecurityRequirementList(ApiTool.getNode(openAPI, "security"), new ArrayList<>());
    String url = null;
    if (ApiTool.hasNode(openAPI, "servers")) {
      final var serversNode = ApiTool.getNode(openAPI, "servers");
      final var urlNode = serversNode.findValue("url");
      if (Objects.nonNull(urlNode)) {
        url = urlNode.textValue();
      }
    }
    final GlobalObjectBuilder globalObject =
        GlobalObject.builder().url(url).authSchemas(authSchemaList).authentications(authList);
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
      // Every scheme of every requirement, once each: a requirement may combine several schemes, and alternatives that share one
      // (e.g. [{tenantId, bearerAuth}, {tenantId, oAuth2}]) must not apply it twice. A scheme without credentials applies nothing.
      securityNode.elements().forEachRemaining(securityRequirement -> securityRequirement.fieldNames().forEachRemaining(schemeName -> {
        if (!authSecList.contains(schemeName)) {
          authSecList.add(schemeName);
        }
      }));
    } else {
      authSecList = authentications;
    }
    return authSecList;
  }

  public static List<PathObject> mapPathObjects(final SpecFile specFile, final Collection<Map<String, JsonNode>> path, final GlobalObject globalObject, final Path baseDir) {
    final List<PathObject> pathObjects = new ArrayList<>();
    for (final var pathMap : path) {
      for (final var pathItem : pathMap.entrySet()) {
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
    for (final Iterator<Entry<String, JsonNode>> it = pathNode.fields(); it.hasNext(); ) {
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
    final JsonNode tagsNode = operation.has("tags") ? operation.get("tags") : null;
    return OperationObject.builder()
                          .operationId(mapOperationId(getOperationId(operation), operationIdList))
                          .operationType(operationType)
                          .summary(ApiTool.getNodeAsString(operation, "summary"))
                          .tags(elementsToStrList(Objects.nonNull(tagsNode) ? tagsNode.elements() : null))
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
    if (Objects.nonNull(tags)) {
      tags.forEachRemaining(tag -> stringList.add(tag.asText()));
    }
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

  /**
   * A request body is required only when the contract says {@code required: true}; OpenAPI defaults it to {@code false}. Reads the
   * resolved body for a {@code $ref}, since {@code required} belongs to the referenced definition.
   */
  private static boolean isRequiredBody(final JsonNode requestBody) {
    return ApiTool.hasNode(requestBody, REQUIRED) && ApiTool.getNode(requestBody, REQUIRED).asBoolean(false);
  }

  private static boolean isInlineMultipart(final JsonNode content) {
    final JsonNode multipartSchema = content.path("multipart/form-data").path(SCHEMA);
    return !multipartSchema.isMissingNode() && !ApiTool.hasRef(multipartSchema);
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
                                        .required(isRequiredBody(requestBody))
                                        .isFormData(ApiTool.getNode(requestBody, CONTENT).has("multipart/form-data"))
                                        .inlineMultipart(isInlineMultipart(ApiTool.getNode(requestBody, CONTENT)))
                                        .contentObjects(mapContentObject(specFile, ApiTool.getNode(requestBody, CONTENT),
                                                                         "InlineObject" + operationIdWithCap, globalObject, baseDir))
                                        .build());
      } else {
        final Optional<JsonNode> requestBodyNode = globalObject.getRequestBodyNode(MapperUtil.getRefSchemaKey(requestBody));
        if (requestBodyNode.isEmpty()) {
          return requestObjects;
        }
        final JsonNode actualRequestBody = requestBodyNode.get();
        requestObjects.add(RequestObject.builder()
                                        .required(isRequiredBody(actualRequestBody))
                                        .isFormData(ApiTool.getNode(actualRequestBody, CONTENT).has("multipart/form-data"))
                                        .inlineMultipart(isInlineMultipart(ApiTool.getNode(actualRequestBody, CONTENT)))
                                        .contentObjects(mapContentObject(specFile, ApiTool.getNode(actualRequestBody, CONTENT),
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
      for (final JsonNode parameter : parameters) {
        if (ApiTool.hasRef(parameter)) {
          final Optional<JsonNode> optRefParameter = globalObject.getParameterNode(MapperUtil.getRefSchemaKey(parameter));
          if (optRefParameter.isEmpty()) {
            continue;
          }
          parameterObjects.addAll(buildParameterObjects(specFile, globalObject, optRefParameter.get(), baseDir));
        } else if (ApiTool.hasNode(parameter, CONTENT)) {
          parameterObjects.addAll(buildParameterContent(contentClassName, parameter, specFile, globalObject, baseDir));
        } else {
          parameterObjects.addAll(buildParameterObjects(specFile, globalObject, parameter, baseDir));
        }
      }
    }
    return parameterObjects;
  }

  /**
   * The parameters a contract parameter is declared as. An object-typed query parameter serialized as exploded {@code form}
   * (the OpenAPI default: {@code ?page_number=1&page_size=20}) or {@code deepObject} ({@code ?filters[page_number]=1}) travels
   * as one query parameter per property, so the server interfaces and the {@code @HttpExchange} interfaces declare one
   * {@code @RequestParam} per property: Spring binds and sends a {@code @RequestParam} as a single value, and could not map
   * the object from its exploded properties. The client classes keep the object, which {@code objectToQueryParams} expands.
   */
  private static List<ParameterObject> buildParameterObjects(
      final SpecFile specFile, final GlobalObject globalObject, final JsonNode parameterNode, final Path baseDir) {
    final ParameterObject parameter = buildParameterObject(specFile, globalObject, parameterNode, baseDir);
    final boolean declaredAsInterface = !specFile.isCallMode() || specFile.isUseHttpExchange();
    final boolean exploded = "deepObject".equals(parameter.getEffectiveStyle())
                             || "form".equals(parameter.getEffectiveStyle()) && parameter.isEffectiveExplode();
    List<ParameterObject> parameters = List.of(parameter);
    if (declaredAsInterface && "query".equals(parameter.getIn()) && exploded) {
      final JsonNode objectSchema = resolveParameterSchema(ApiTool.getNode(parameterNode, SCHEMA), specFile, globalObject, baseDir);
      if (Objects.nonNull(objectSchema) && ApiTool.hasProperties(objectSchema) && !ApiTool.hasAdditionalProperties(objectSchema)) {
        parameters = explodeObjectParameter(parameter, objectSchema, specFile, globalObject, baseDir);
      }
    }
    return parameters;
  }

  private static JsonNode resolveParameterSchema(final JsonNode schema, final SpecFile specFile, final GlobalObject globalObject, final Path baseDir) {
    JsonNode resolved = schema;
    if (Objects.nonNull(schema) && ApiTool.hasRef(schema)) {
      resolved = getRefSchema(schema, specFile, globalObject, baseDir, MapperUtil.getRefSchemaName(schema, null));
    }
    return resolved;
  }

  private static List<ParameterObject> explodeObjectParameter(
      final ParameterObject parameter, final JsonNode objectSchema, final SpecFile specFile, final GlobalObject globalObject, final Path baseDir) {
    final boolean deepObject = "deepObject".equals(parameter.getEffectiveStyle());
    final List<ParameterObject> properties = new ArrayList<>();
    ApiTool.getProperties(objectSchema).forEachRemaining(property -> {
      final JsonNode propertySchema = property.getValue();
      final var dataType = getSchemaType(propertySchema, TypeConstants.OBJECT, specFile, globalObject, baseDir);
      properties.add(ParameterObject.builder()
                                    .name(deepObject ? parameter.getName() + "[" + property.getKey() + "]" : property.getKey())
                                    .in("query")
                                    .required(Boolean.TRUE.equals(parameter.getRequired()) && ApiTool.checkIfRequired(objectSchema, property.getKey()))
                                    .description(ApiTool.getNodeAsString(propertySchema, DESCRIPTION))
                                    .dataType(dataType)
                                    .isCollection(ApiTool.hasItems(propertySchema))
                                    .importName(getParameterImport(dataType))
                                    .defaultValue(getDefaultValue(propertySchema))
                                    .camelCaseName(specFile.isUseCamelCaseNames())
                                    .build());
    });
    return properties;
  }

  private static ParameterObject buildParameterObject(
      final SpecFile specFile, final GlobalObject globalObject, final JsonNode refParameter, final Path baseDir) {
    final var dateType = getSchemaType(getContentOrSchema(refParameter), TypeConstants.OBJECT, specFile, globalObject, baseDir);
    return ParameterObject.builder()
                          .name(ApiTool.getName(refParameter))
                          .required(ApiTool.getNodeAsBoolean(refParameter, REQUIRED))
                          .description(ApiTool.getNodeAsString(refParameter, DESCRIPTION))
                          .in(ApiTool.getNodeAsString(refParameter, "in"))
                          .dataType(dateType)
                          .isCollection(ApiTool.hasItems(getContentOrSchema(refParameter)))
                          .importName(getParameterImport(dateType))
                          .defaultValue(getDefaultValue(ApiTool.getNode(refParameter, SCHEMA)))
                          .camelCaseName(specFile.isUseCamelCaseNames())
                          .style(ApiTool.getNodeAsString(refParameter, "style"))
                          .explode(refParameter.has("explode") ? refParameter.get("explode").asBoolean() : null)
                          .build();
  }

  /** The parameter schema's scalar {@code default} as text, or null when it declares none. */
  private static String getDefaultValue(final JsonNode schema) {
    final JsonNode defaultNode = Objects.nonNull(schema) ? schema.get("default") : null;
    return Objects.nonNull(defaultNode) && defaultNode.isValueNode() && !defaultNode.isNull() ? defaultNode.asText() : null;
  }

  /**
   * The class the API has to import to declare a parameter of this type: the model the parameter resolves to, or, when the type is one the language already
   * provides, its JDK class. A parameter typed after a schema used to report the JDK mapping of its base type, which is null for every model, so the API was
   * generated referring to a model it never imported.
   */
  private static String getParameterImport(final SchemaFieldObjectType dataType) {
    final String modelImport = getImportFromType(dataType);
    return Objects.nonNull(modelImport) ? modelImport : dataType.getImportName();
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
    for (final Iterator<JsonNode> it = content.elements(); it.hasNext(); ) {
      final var contentType = it.next();
      final String inlineParameter = INLINE_PARAMETER + safeCapitalize(contentClassName)
                                     + StringUtils.capitalize(parameterName);

      final String inlineParameterPojo = getPojoName(inlineParameter, specFile);
      final var builder = ParameterObject.builder()
                                         .name(parameterName)
                                         .required(ApiTool.getNodeAsBoolean(parameter, REQUIRED))
                                         .description(ApiTool.getNodeAsString(parameter, DESCRIPTION))
                                         .in(ApiTool.getNodeAsString(parameter, "in"))
                                         .camelCaseName(specFile.isUseCamelCaseNames());
      final var parameterSchema = ApiTool.getNode(contentType, SCHEMA);
      if (TypeConstants.OBJECT.equalsIgnoreCase(ApiTool.getType(parameterSchema))) {
        parameterObjects.add(builder
                                 .name(parameterName)
                                 .dataType(SchemaFieldObjectType.fromTypeList(inlineParameterPojo))
                                 .importName(inlineParameterPojo)
                                 .build());
        globalObject.getSchemaMap().put(StringCaseUtils.titleToSnakeCase(inlineParameterPojo), parameterSchema);
      } else {
        final var dataType = getSchemaType(parameterSchema, inlineParameterPojo, specFile, globalObject, baseDir);
        parameterObjects.add(builder
                                 .name(parameterName)
                                 .dataType(dataType)
                                 .importName(getParameterImport(dataType))
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
    JsonNode realResponse = response;
    if (ApiTool.hasRef(response)) {
      final String refValue = ApiTool.getRefValue(response);
      if (refValue.startsWith("#")) {
        final Optional<JsonNode> resolvedResponse = globalObject.getResponseNode(MapperUtil.getRefSchemaKey(response));
        if (resolvedResponse.isPresent()) {
          realResponse = resolvedResponse.get();
        } else {
          try {
            final URI baseUri = baseDir.resolve(specFile.getFilePath()).getParent().toUri();
            realResponse = SchemaUtil.solveRef(refValue, globalObject.getResponseMap(), baseUri);
          } catch (final Exception e) {
            realResponse = null;
          }
        }
      } else {
        try {
          final URI baseUri = baseDir.resolve(specFile.getFilePath()).getParent().toUri();
          realResponse = SchemaUtil.loadAndResolveRefs(baseUri, refValue);
        } catch (final Exception e) {
          realResponse = null;
        }
      }
    }
    final String operationIdWithCap = operationId.substring(0, 1).toUpperCase() + operationId.substring(1);
    final JsonNode content = Objects.nonNull(realResponse) ? ApiTool.getNode(realResponse, CONTENT) : null;
    final String description = Objects.nonNull(realResponse) ? StringUtils.defaultIfEmpty(ApiTool.getNodeAsString(realResponse, DESCRIPTION), "") : "";
    responseObjects.add(ResponseObject
                            .builder()
                            .responseName(responseCode)
                            .description(description)
                            .contentObjects(mapContentObject(specFile, content, "InlineResponse" + responseCode + operationIdWithCap, globalObject, baseDir))
                            .build());
  }

  private static List<ContentObject> mapContentObject(
      final SpecFile specFile, final JsonNode content, final String inlineObject, final GlobalObject globalObject,
      final Path baseDir) {
    final List<ContentObject> contentObjects = new ArrayList<>();
    if (Objects.nonNull(content)) {
      for (final Iterator<String> it = content.fieldNames(); it.hasNext(); ) {
        final String mediaType = it.next();
        final var schema = ApiTool.getNode(ApiTool.getNode(content, mediaType), SCHEMA);
        final String pojoName = preparePojoName(inlineObject, schema, specFile);
        final SchemaFieldObjectType dataType = getSchemaType(schema, pojoName, specFile, globalObject, baseDir);
        final String importName = getImportFromType(dataType);
        SchemaObject schemaObject = null;
        if (mediaType.equals("application/x-www-form-urlencoded") || mediaType.equals("multipart/form-data")) {
          schemaObject = MapperContentUtil.mapComponentToSchemaObject(globalObject.getSchemaMap(), pojoName, schema, dataType.getBaseType(), specFile, baseDir).get(0);
          if (specFile.isUseCamelCaseNames() && Objects.nonNull(schemaObject.getFieldObjectList())) {
            // The client reads the form fields through the model's getters, which are camel case too.
            schemaObject.getFieldObjectList().forEach(field -> field.setCamelCaseName(true));
          }
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
    // Use unified ResponseWrapperHandler for wrapper decisions (v7.0)
    // This ensures OpenApiUtil and MapperPathUtil stay in sync

    if (ResponseWrapperHandler.shouldCreateWrapper(schema)) {
      // Wrapper will be created by ResponseWrapperHandler; use the wrapper name
      // This is called with inlineObject = "InlineResponse{code}{operationId}"
      // ResponseWrapperHandler will create the same name, so just use it
      return getPojoName(inlineObject, specFile);
    }

    // No wrapper: use the schema directly
    if (ApiTool.hasRef(schema)) {
      return getPojoName(inlineObject + MapperUtil.getRefSchemaName(schema, null), specFile);
    } else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
      final var items = ApiTool.getItems(schema);
      if (ApiTool.hasRef(items)) {
        return getPojoName(inlineObject + MapperUtil.getRefSchemaName(items, null), specFile);
      }
    }

    return getPojoName(inlineObject, specFile);
  }

  private static SchemaFieldObjectType getSchemaType(
      final JsonNode schema, final String pojoName, final SpecFile specFile, final GlobalObject globalObject,
      final Path baseDir) {
    SchemaFieldObjectType type = null;

    if (ApiTool.hasRef(schema)) {
      final String refSchemaPojoName = MapperUtil.getPojoNameFromRef(schema, specFile, pojoName);
      final JsonNode refSchema = getRefSchema(schema, specFile, globalObject, baseDir, pojoName);
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

  private static JsonNode getRefSchema(JsonNode schema, SpecFile specFile, GlobalObject globalObject, Path baseDir, String inlinePojoName) {
    JsonNode refSchema;
    final String refValue = ApiTool.getRefValue(schema);
    if (refValue.contains("schemas")) {
      refSchema = SchemaUtil.solveRef(refValue, globalObject.getSchemaMap(),
                                      baseDir.resolve(specFile.getFilePath()).getParent().toUri());
      if (Objects.nonNull(refSchema) && !refValue.contains("#")) {
        final String key = resolveSchemaMapKey(refValue, refSchema, inlinePojoName);
        globalObject.getSchemaMap().put(key, refSchema);
      }
    } else if (refValue.contains("requestBodies")) {
      refSchema = SchemaUtil.solveRef(refValue, globalObject.getRequestBodyMap(),
                                      baseDir.resolve(specFile.getFilePath()).getParent().toUri());
    } else if (refValue.contains("parameters")) {
      refSchema = SchemaUtil.solveRef(refValue, globalObject.getParameterMap(),
                                      baseDir.resolve(specFile.getFilePath()).getParent().toUri());
    } else if (refValue.contains("responseBodies")) {
      refSchema = SchemaUtil.solveRef(refValue, globalObject.getResponseMap(),
                                      baseDir.resolve(specFile.getFilePath()).getParent().toUri());
    } else {
      refSchema = SchemaUtil.solveRef(refValue, globalObject.getSchemaMap(),
                                      baseDir.resolve(specFile.getFilePath()).getParent().toUri());
      globalObject.getSchemaMap().put(inlinePojoName, refSchema);
    }
    return refSchema;
  }

  private static String resolveSchemaMapKey(final String refValue, final JsonNode resolvedSchema, final String inlinePojoName) {
    if (StringUtils.isNotEmpty(refValue) && !refValue.startsWith("#") && !refValue.contains("#")
        && !StringUtils.startsWith(inlinePojoName, "Inline")
        && Objects.nonNull(resolvedSchema) && !ApiTool.hasComponents(resolvedSchema)
        && (ApiTool.hasType(resolvedSchema) || ApiTool.isComposed(resolvedSchema) || ApiTool.isEnum(resolvedSchema))) {
      final String fileKey = SchemaUtil.computeFileSchemaKey(refValue);
      return StringUtils.defaultIfEmpty(fileKey, inlinePojoName);
    }
    return inlinePojoName;
  }

  private static SchemaFieldObjectType getObjectOrType(
      final JsonNode schema, final String pojoName, final SpecFile specFile, final GlobalObject globalObject,
      final Path baseDir) {
    if (ApiTool.isBinary(schema)) {
      // A raw binary body (e.g. application/octet-stream, type: string + format: binary,
      // not wrapped in a multipart object) maps to a streaming Resource, not String.
      return new SchemaFieldObjectType(TypeConstants.RESOURCE);
    }
    return switch (ApiTool.getType(schema)) {
      case TypeConstants.OBJECT -> SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
      case TypeConstants.INTEGER -> new SchemaFieldObjectType(getIntegerFormat(schema));
      case TypeConstants.NUMBER -> new SchemaFieldObjectType(getNumberFormat(schema));
      case TypeConstants.BOOLEAN -> new SchemaFieldObjectType(TypeConstants.BOOLEAN);
      case TypeConstants.ARRAY -> new SchemaFieldObjectType(TypeConstants.ARRAY, getSchemaType(ApiTool.getItems(schema), pojoName, specFile, globalObject, baseDir));
      default -> new SchemaFieldObjectType(TypeConstants.STRING);
    };
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
