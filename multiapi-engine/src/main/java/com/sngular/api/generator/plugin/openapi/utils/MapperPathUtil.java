/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.sngular.api.generator.plugin.openapi.exception.DuplicatedOperationException;
import com.sngular.api.generator.plugin.openapi.model.AuthSchemaObject;
import com.sngular.api.generator.plugin.openapi.model.ContentObject;
import com.sngular.api.generator.plugin.openapi.model.GlobalObject;
import com.sngular.api.generator.plugin.openapi.model.OperationObject;
import com.sngular.api.generator.plugin.openapi.model.ParameterObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import com.sngular.api.generator.plugin.openapi.model.RequestObject;
import com.sngular.api.generator.plugin.openapi.model.ResponseObject;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.openapi.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.DateSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import org.apache.commons.lang3.StringUtils;

public class MapperPathUtil {

  public static final String INLINE_PARAMETER = "InlineParameter";

  private MapperPathUtil() {}

  public static GlobalObject mapOpenApiObjectToOurModels(final OpenAPI openAPI, final List<AuthSchemaObject> authSchemaList) {
    final var authList = getSecurityRequirementList(openAPI.getSecurity(), new ArrayList<>());
    return GlobalObject.builder()
                       .url(openAPI.getServers().get(0).getUrl())
                       .authSchemas(authSchemaList)
                       .authentications(authList)
                       .schemaMap(openAPI.getComponents().getSchemas())
                       .build();
  }

  private static List<String> getSecurityRequirementList(final List<SecurityRequirement> securityRequirementList, final List<String> authentications) {
    final List<String> authSecList;
    if (null != securityRequirementList && !securityRequirementList.isEmpty()) {
      authSecList = new ArrayList<>();
      securityRequirementList.forEach(securityRequirement -> securityRequirement.forEach((key, value) -> authSecList.add(key)));
    } else {
      authSecList = authentications;
    }
    return authSecList;
  }

  public static List<PathObject> mapPathObjects(
      final OpenAPI openAPI, final SpecFile specFile, final Entry<String, HashMap<String, PathItem>> path,
      final GlobalObject globalObject) {
    final List<PathObject> pathObjects = new ArrayList<>();
    for (Entry<String, PathItem> pathItem : path.getValue().entrySet()) {
      final PathObject pathObject = PathObject.builder()
                                              .pathName(pathItem.getKey())
                                              .globalObject(globalObject)
                                              .operationObjects(mapOperationObject(openAPI, specFile, pathItem, globalObject))
                                              .build();
      pathObjects.add(pathObject);
    }

    return pathObjects;
  }

  private static List<OperationObject> mapOperationObject(final OpenAPI openAPI, final SpecFile specFile, final Entry<String, PathItem> path, final GlobalObject globalObject) {
    final List<OperationObject> operationObjects = new ArrayList<>();
    final List<String> operationIdList = new ArrayList<>();
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getGet()))) {
      operationObjects.add(createOperation(openAPI, path.getValue().getGet(), "GET", specFile, globalObject, operationIdList));
    }
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getPost()))) {
      operationObjects.add(createOperation(openAPI, path.getValue().getPost(), "POST", specFile, globalObject, operationIdList));
    }
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getDelete()))) {
      operationObjects.add(createOperation(openAPI, path.getValue().getDelete(), "DELETE", specFile, globalObject, operationIdList));
    }
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getPut()))) {
      operationObjects.add(createOperation(openAPI, path.getValue().getPut(), "PUT", specFile, globalObject, operationIdList));
    }
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getPatch()))) {
      operationObjects.add(createOperation(openAPI, path.getValue().getPatch(), "PATCH", specFile, globalObject, operationIdList));
    }

    return operationObjects;
  }

  private static OperationObject createOperation(
      final OpenAPI openAPI, final Operation operation, final String operationType,
      final SpecFile specFile, final GlobalObject globalObject, final List<String> operationIdList) {
    return OperationObject.builder()
                          .operationId(mapOperationId(operation.getOperationId(), operationIdList))
                          .operationType(operationType)
                          .summary(operation.getSummary())
                          .tags(operation.getTags())
                          .requestObjects(mapRequestObject(specFile, operation, globalObject))
                          .responseObjects(mapResponseObject(specFile, operation, globalObject))
                          .parameterObjects(mapParameterObjects(openAPI, operation.getParameters(), specFile, operation.getOperationId(), globalObject))
                          .securities(getSecurityRequirementList(operation.getSecurity(), globalObject.getAuthentications()))
                          .consumes(getConsumesList(operation.getRequestBody()))
                          .produces(getProducesList(operation.getResponses()))
                          .build();
  }

  private static String mapOperationId(final String operationId, final List<String> operationIdList) {
    if (operationIdList.contains(operationId)) {
      throw new DuplicatedOperationException(operationId);
    } else {
      operationIdList.add(operationId);
      return operationId;
    }

  }

  private static List<String> getConsumesList(final RequestBody requestBody) {
    final var consumesList = new ArrayList<String>();
    if (Objects.nonNull(requestBody) && Objects.nonNull(requestBody.getContent())
        && !requestBody.getContent().isEmpty()) {

      final Set<String> consumes = requestBody.getContent().keySet();
      consumes.forEach(key -> {
        if (!key.equalsIgnoreCase("*/*")) {
          consumesList.add(key.replace("\"", "\\\""));
        }
      });
    }

    return consumesList;
  }

  private static List<String> getProducesList(final ApiResponses responses) {
    final var producesList = new ArrayList<String>();

    if (Objects.nonNull(responses) && !responses.isEmpty()) {
      responses.forEach((key1, value) -> {
        if (Objects.nonNull(value) && Objects.nonNull(value.getContent()) && !value.getContent().isEmpty()) {
          final Set<String> produces = value.getContent().keySet();
          produces.forEach(key -> {
            if (!key.equalsIgnoreCase("*/*") && !producesList.contains(key)) {
              producesList.add(key.replace("\"", "\\\""));
            }
          });
        }
      });
    }
    return producesList;
  }

  private static List<RequestObject> mapRequestObject(final SpecFile specFile, final Operation operation, final GlobalObject globalObject) {
    final List<RequestObject> requestObjects = new ArrayList<>();
    final String operationIdWithCap = operation.getOperationId().substring(0, 1).toUpperCase() + operation.getOperationId().substring(1);
    if (Objects.nonNull(operation.getRequestBody())) {
      requestObjects.add(RequestObject.builder()
                                      .required(operation.getRequestBody().getRequired())
                                      .contentObjects(mapContentObject(specFile, operation.getRequestBody().getContent(), "InlineObject" + operationIdWithCap, globalObject))
                                      .build());
    }
    return requestObjects;
  }

  private static List<ParameterObject> mapParameterObjects(
      final OpenAPI openAPI, final List<Parameter> parameters, final SpecFile specFile, final String contentClassName,
      final GlobalObject globalObject) {
    final List<ParameterObject> parameterObjects = new ArrayList<>();
    if (Objects.nonNull(parameters) && !parameters.isEmpty()) {
      for (Parameter parameter : parameters) {
        if (Objects.nonNull(parameter.get$ref())) {
          final String[] wholeRef = parameter.get$ref().split("/");
          final String ref = wholeRef[wholeRef.length - 1];
          final Parameter refParameter = openAPI.getComponents().getParameters().get(ref);
          parameterObjects.add(ParameterObject.builder()
                                              .name(refParameter.getName())
                                              .required(refParameter.getRequired())
                                              .description(refParameter.getDescription())
                                              .in(refParameter.getDescription())
                                              .dataType(getSchemaType(parameter.getSchema(), TypeConstants.OBJECT, specFile, globalObject))
                                              .isCollection(TypeConstants.ARRAY.equalsIgnoreCase(refParameter.getSchema().getType()))
                                              .build());
        } else if (Objects.nonNull(parameter.getContent())) {
          addInlineParametersToList(contentClassName, parameterObjects, parameter, specFile, globalObject);
        } else {
          parameterObjects.add(ParameterObject.builder()
                                              .name(parameter.getName())
                                              .required(parameter.getRequired())
                                              .description(parameter.getDescription())
                                              .in(parameter.getIn())
                                              .dataType(getSchemaType(parameter.getSchema(), TypeConstants.OBJECT, specFile, globalObject))
                                              .isCollection(TypeConstants.ARRAY.equalsIgnoreCase(parameter.getSchema().getType()))
                                              .build());
        }
      }
    }
    return parameterObjects;
  }

  private static void addInlineParametersToList(
      final String contentClassName, final List<ParameterObject> parameterObjects, final Parameter parameter, final SpecFile specFile,
      final GlobalObject globalObject) {
    final Content content = parameter.getContent();
    for (Entry<String, MediaType> contentEntrySet : content.entrySet()) {
      final String inlineParameter = getPojoName(INLINE_PARAMETER + StringUtils.capitalize(contentClassName)
                                                 + StringUtils.capitalize(parameter.getName()), specFile);
      if (TypeConstants.OBJECT.equalsIgnoreCase(contentEntrySet.getValue().getSchema().getType())) {
        parameterObjects.add(ParameterObject.builder()
                                            .name(parameter.getName())
                                            .required(parameter.getRequired())
                                            .description(parameter.getDescription())
                                            .in(parameter.getIn())
                                            .dataType(SchemaFieldObjectType.fromTypeList(inlineParameter))
                                            .importName(inlineParameter)
                                            .build());
      } else {
        parameterObjects.add(ParameterObject.builder()
                                            .name(parameter.getName())
                                            .required(parameter.getRequired())
                                            .description(parameter.getDescription())
                                            .in(parameter.getIn())
                                            .dataType(getSchemaType(contentEntrySet.getValue().getSchema(), inlineParameter, specFile, globalObject))
                                            .build());
      }
    }
  }

  private static List<ResponseObject> mapResponseObject(final SpecFile specFile, final Operation operation, final GlobalObject globalObject) {
    final List<ResponseObject> responseObjects = new ArrayList<>();
    final ApiResponses responses = operation.getResponses();
    if (Objects.nonNull(responses)) {
      responses.forEach((key, value) -> {
        final String operationIdWithCap = operation.getOperationId().substring(0, 1).toUpperCase() + operation.getOperationId().substring(1);
        responseObjects.add(ResponseObject.builder()
                                          .responseName(key)
                                          .description(value.getDescription())
                                          .contentObjects(mapContentObject(specFile, value.getContent(), "InlineResponse" + key + operationIdWithCap, globalObject))
                                          .build());
      });
    }
    return responseObjects;
  }

  private static List<ContentObject> mapContentObject(final SpecFile specFile, final Content content, final String inlineObject, final GlobalObject globalObject) {
    final List<ContentObject> contentObjects = new ArrayList<>();
    if (Objects.nonNull(content)) {
      for (Entry<String, MediaType> mediaTypeEntry : content.entrySet()) {
        final Schema<?> schema = mediaTypeEntry.getValue().getSchema();
        final String pojoName = preparePojoName(inlineObject, schema, specFile);
        final SchemaFieldObjectType dataType = getSchemaType(schema, pojoName, specFile, globalObject);
        final String importName = getImportFromType(dataType);
        contentObjects.add(ContentObject.builder()
                                        .dataType(dataType)
                                        .name(mediaTypeEntry.getKey())
                                        .importName(importName)
                                        .build());
      }
    }
    return contentObjects;
  }

  private static String preparePojoName(final String inlineObject, final Schema<?> schema, final SpecFile specFile) {
    final String pojoName;
    if (Objects.nonNull(schema.getAllOf())) {
      pojoName = getPojoName(inlineObject + "AllOf", specFile);
    } else if (Objects.nonNull(schema.getAnyOf())) {
      pojoName = getPojoName(inlineObject + "AnyOf", specFile);
    } else if (Objects.nonNull(schema.getOneOf())) {
      pojoName = getPojoName(inlineObject + "OneOf", specFile);
    } else {
      pojoName = getPojoName(inlineObject, specFile);
    }

    return pojoName;
  }

  private static SchemaFieldObjectType getSchemaType(final Schema<?> schema, final String pojoName, final SpecFile specFile, final GlobalObject globalObject) {
    final SchemaFieldObjectType type;

    if (Objects.nonNull(schema.get$ref())) {
      final String refSchemaName = MapperContentUtil.cleanRefName(schema);
      final String refSchemaPojoName = MapperContentUtil.getRef(schema, specFile);
      final Schema<?> refSchema = globalObject.getSchemaMap().get(refSchemaName);
      type = getSchemaType(refSchema, refSchemaPojoName, specFile, globalObject);
    } else if (schema instanceof MapSchema) {
      type = getMapSchemaType((MapSchema) schema, pojoName, specFile, globalObject);
    } else if (schema instanceof DateSchema) {
      type = new SchemaFieldObjectType(MapperContentUtil.getDateType(specFile));
    } else if (Objects.isNull(schema.getType()) || schema.getType().equalsIgnoreCase(TypeConstants.OBJECT)) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
    } else {
      switch (schema.getType()) {
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
          type = new SchemaFieldObjectType(TypeConstants.ARRAY, getSchemaType(schema.getItems(), pojoName, specFile, globalObject));
          break;
        default:
          type = new SchemaFieldObjectType(TypeConstants.STRING);
      }
    }

    return type;
  }

  private static SchemaFieldObjectType getMapSchemaType(final MapSchema schema, final String pojoName, final SpecFile specFile, final GlobalObject globalObject) {
    final SchemaFieldObjectType type;

    final Object addPropObj = schema.getAdditionalProperties();
    if (!schema.getProperties().isEmpty()) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
    } else if (addPropObj instanceof Boolean) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.OBJECT);
    } else {
      type = new SchemaFieldObjectType(TypeConstants.MAP, getSchemaType((Schema<?>) addPropObj, pojoName, specFile, globalObject));
    }

    return type;
  }

  private static String getIntegerFormat(final Schema<?> schema) {
    return TypeConstants.INT_64.equalsIgnoreCase(schema.getFormat()) ? TypeConstants.LONG : TypeConstants.INTEGER;
  }

  private static String getNumberFormat(final Schema<?> schema) {
    final String typeName;
    if (TypeConstants.FLOAT.equalsIgnoreCase(schema.getFormat())) {
      typeName = TypeConstants.FLOAT;
    } else if (TypeConstants.DOUBLE.equalsIgnoreCase(schema.getFormat())) {
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

  private static Boolean checkIfOperationIsNull(final Operation operation) {
    return Objects.nonNull(operation);
  }

  public static String getPojoName(final String namePojo, final SpecFile specFile) {
    return (StringUtils.isNotBlank(specFile.getModelNamePrefix()) ? specFile.getModelNamePrefix() : "")
           + namePojo
           + (StringUtils.isNotBlank(specFile.getModelNameSuffix()) ? specFile.getModelNameSuffix() : "");
  }

}
