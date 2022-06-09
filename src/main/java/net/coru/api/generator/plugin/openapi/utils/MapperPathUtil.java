/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import static net.coru.api.generator.plugin.openapi.utils.MapperUtil.getPojoName;
import static net.coru.api.generator.plugin.openapi.utils.MapperUtil.getSimpleType;
import static net.coru.api.generator.plugin.openapi.utils.MapperUtil.getTypeArray;
import static net.coru.api.generator.plugin.openapi.utils.MapperUtil.getTypeMap;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import io.swagger.v3.oas.models.parameters.Parameter;
import net.coru.api.generator.plugin.exception.SCSMultiApiMavenPluginException;
import net.coru.api.generator.plugin.openapi.model.AuthSchemaObject;
import net.coru.api.generator.plugin.openapi.model.BasicTypeConstants;
import net.coru.api.generator.plugin.openapi.model.ContentObject;
import net.coru.api.generator.plugin.openapi.model.GlobalObject;
import net.coru.api.generator.plugin.openapi.model.OperationObject;
import net.coru.api.generator.plugin.openapi.model.ParameterObject;
import net.coru.api.generator.plugin.openapi.model.PathObject;
import net.coru.api.generator.plugin.openapi.model.RefNameObject;
import net.coru.api.generator.plugin.openapi.model.RequestObject;
import net.coru.api.generator.plugin.openapi.model.ResponseObject;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import org.apache.commons.lang3.StringUtils;

public class MapperPathUtil {

  public static GlobalObject mapOpenApiObjectToOurModels(OpenAPI openAPI, FileSpec fileSpec, List<AuthSchemaObject> authSchemaList) {

    var authList = getSecurityRequirementList(openAPI.getSecurity(), new ArrayList<>());

    return GlobalObject.builder()
                       .url(openAPI.getServers().get(0).getUrl())
                       .version(openAPI.getInfo().getVersion())
                       .title(openAPI.getInfo().getTitle())
                       .license(openAPI.getInfo().getLicense().getName())
                       .authSchemas(authSchemaList)
                       .authentications(authList)
                       .componentsTypeMap(getMapComponentsTypes(openAPI.getComponents(), fileSpec))
                       .build();
  }

  private static HashMap<String, String> getMapComponentsTypes(Components components, FileSpec fileSpec) {
    var mapComponents = new HashMap<String, String>();

    if (null == components.getSchemas() || components.getSchemas().isEmpty()) {
      return mapComponents;
    }
    components.getSchemas().forEach((key, value) -> {
      if (!mapComponents.containsKey(key)) {
        var type = checkSchemaType(value, fileSpec);
        mapComponents.put(key, type.equalsIgnoreCase("object") ? getPojoName(key, fileSpec) : type);
      }
    });
    return mapComponents;
  }

  private static String checkSchemaType(Schema schema, FileSpec fileSpec) {
    var dataType = schema.getType();

    if (schema instanceof ArraySchema) {
      dataType = "array-" + getTypeArray((ArraySchema) schema, fileSpec);
    } else if (schema instanceof MapSchema) {
      dataType = "map-" + getTypeMap((MapSchema) schema, fileSpec);
    } else if (schema.getType().equals("object") && StringUtils.isNotBlank(schema.get$ref())) {
      String[] pathObjectRef = schema.get$ref().split("/");
      dataType = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
    }

    return dataType;
  }

  public static ArrayList<PathObject> mapPathObjects(FileSpec fileSpec, Entry<String, HashMap<String, PathItem>> path, GlobalObject globalObject) {
    ArrayList<PathObject> pathObjects = new ArrayList<>();
    for (Entry<String, PathItem> pathItem : path.getValue().entrySet()) {
      if (Objects.nonNull(pathItem.getValue().getParameters())) {
        globalObject.setParameterObjects(mapGlobalParameterObjects(pathItem.getValue().getParameters()));
      }
      PathObject pathObject = PathObject.builder()
                                        .pathName(pathItem.getKey())
                                        .globalObjects(globalObject)
                                        .operationObject(mapOperationObject(fileSpec, pathItem, globalObject))
                                        .build();
      pathObjects.add(pathObject);
    }

    return pathObjects;
  }

  private static List<OperationObject> mapOperationObject(FileSpec fileSpec, Entry<String, PathItem> path, GlobalObject globalObject) {
    List<OperationObject> operationObjects = new ArrayList<>();
    List<String> operationIdList = new ArrayList<>();
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getGet()))) {
      operationObjects.add(createOperation(path.getValue().getGet(), "GET", fileSpec, globalObject, operationIdList));
    }
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getPost()))) {
      operationObjects.add(createOperation(path.getValue().getPost(), "POST", fileSpec, globalObject, operationIdList));
    }
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getDelete()))) {
      operationObjects.add(createOperation(path.getValue().getDelete(), "DELETE", fileSpec, globalObject, operationIdList));
    }
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getPut()))) {
      operationObjects.add(createOperation(path.getValue().getPut(), "PUT", fileSpec, globalObject, operationIdList));
    }
    if (Boolean.TRUE.equals(checkIfOperationIsNull(path.getValue().getPatch()))) {
      operationObjects.add(createOperation(path.getValue().getPatch(), "PATCH", fileSpec, globalObject, operationIdList));
    }

    return operationObjects;
  }

  private static OperationObject createOperation(
    Operation operation, String operationType, FileSpec fileSpec, GlobalObject globalObject, final List<String> operationIdList) {
    return OperationObject.builder()
                          .operationId(mapOperationId(operation.getOperationId(), operationIdList))
                          .operationType(operationType)
                          .summary(operation.getSummary())
                          .tags(operation.getTags())
                          .requestObjects(mapRequestObject(fileSpec, operation, globalObject))
                          .responseObjects(mapResponseObject(fileSpec, operation.getResponses(), globalObject))
                          .parameterObjects(mapParameterObjects(operation.getParameters(), globalObject))
                          .security(getSecurityRequirementList(operation.getSecurity(), globalObject.getAuthentications()))
                          .consumes(getConsumesList(operation.getRequestBody()))
                          .produces(getProducesList(operation.getResponses()))
                          .build();
  }

  private static String mapOperationId(final String operationId, final List<String> operationIdList) {
    if (operationIdList.contains(operationId)) {
      throw new SCSMultiApiMavenPluginException("Do not write the same operationId twice");
    } else {
      operationIdList.add(operationId);
      return operationId;
    }

  }

  private static List<String> getConsumesList(RequestBody requestBody) {
    var consumesList = new ArrayList<String>();
    if (requestBody != null && requestBody.getContent() != null
        && !requestBody.getContent().isEmpty()) {

      Set<String> consumes = requestBody.getContent().keySet();
      consumes.forEach(key -> {
        if (!key.equalsIgnoreCase("*/*")) {
          consumesList.add(key.replace("\"", "\\\""));
        }
      });
    }

    return consumesList;
  }

  private static List<String> getProducesList(ApiResponses responses) {
    var producesList = new ArrayList<String>();

    if (!Objects.nonNull(responses) || responses.isEmpty()) {
      return producesList;
    }

    responses.entrySet().forEach(inputResponse -> {
      if (inputResponse.getValue() != null && inputResponse.getValue().getContent() != null
          && !inputResponse.getValue().getContent().isEmpty()) {

        Set<String> produces = inputResponse.getValue().getContent().keySet();
        produces.forEach(key -> {
          if (!key.equalsIgnoreCase("*/*") && !producesList.contains(key)) {
            producesList.add(key.replace("\"", "\\\""));
          }
        });
      }
    });

    return producesList;
  }

  private static List<RequestObject> mapRequestObject(FileSpec fileSpec, Operation operation, GlobalObject globalObject) {
    List<RequestObject> requestObjects = new ArrayList<>();
    String firstLetter = operation.getOperationId().substring(0, 1);
    String remainingLetters = operation.getOperationId().substring(1);

    String operationId = firstLetter.toUpperCase() + remainingLetters;
    if (Objects.nonNull(operation.getRequestBody())) {
      requestObjects.add(RequestObject.builder()
                                      .description(operation.getRequestBody().getDescription())
                                      .required(operation.getRequestBody().getRequired())
                                      .contentObject(mapContentObject(fileSpec, operation.getRequestBody().getContent(), "InlineObject" + operationId, globalObject))
                                      .build());
    }
    return requestObjects;
  }

  private static List<ParameterObject> mapParameterObjects(final List<Parameter> parameters, GlobalObject globalObject) {
    List<ParameterObject> parameterObjects = new ArrayList<>();
    if (Objects.nonNull(globalObject.getParameterObjects())) {
      if (Objects.nonNull(globalObject.getParameterObjects()) && Objects.nonNull(parameters)) {
        throw new SCSMultiApiMavenPluginException("Defining parameters in both Path and Operations is not supported in this plugin");
      }
      globalObject.getParameterObjects().forEach(parameter -> parameterObjects.add(ParameterObject.builder()
                                                                                                  .name(parameter.getName())
                                                                                                  .required(parameter.getRequired())
                                                                                                  .description(parameter.getDescription())
                                                                                                  .in(parameter.getIn())
                                                                                                  .className(parameter.getClassName())
                                                                                                  .isCollection(parameter.getIsCollection())
                                                                                                  .build()));
    } else if (Objects.nonNull(parameters)) {
      parameters.forEach(parameter -> parameterObjects.add(ParameterObject.builder()
                                                                          .name(parameter.getName())
                                                                          .required(parameter.getRequired())
                                                                          .description(parameter.getDescription())
                                                                          .in(parameter.getIn())
                                                                          .className(getSimpleType(parameter.getSchema()))
                                                                          .isCollection(parameter.getSchema().getType().equalsIgnoreCase("array"))
                                                                          .build()));
    }
    return parameterObjects;
  }

  private static List<ParameterObject> mapGlobalParameterObjects(final List<Parameter> parameters) {
    List<ParameterObject> parameterObjects = new ArrayList<>();
    if (Objects.nonNull(parameters)) {
      parameters.forEach(parameter -> parameterObjects.add(ParameterObject.builder()
                                                                          .name(parameter.getName())
                                                                          .required(parameter.getRequired())
                                                                          .description(parameter.getDescription())
                                                                          .in(parameter.getIn())
                                                                          .className(getSimpleType(parameter.getSchema()))
                                                                          .isCollection(parameter.getSchema().getType().equalsIgnoreCase("array"))
                                                                          .build()));
    }

    return parameterObjects;
  }

  private static List<ResponseObject> mapResponseObject(FileSpec fileSpec, ApiResponses responses, GlobalObject globalObject) {
    List<ResponseObject> responseObjects = new ArrayList<>();
    if (Objects.nonNull(responses)) {
      for (Entry<String, ApiResponse> response : responses.entrySet()) {
        responseObjects.add(ResponseObject.builder()
                                          .responseName(response.getKey())
                                          .description(response.getValue().getDescription())
                                          .contentObject(mapContentObject(fileSpec, response.getValue().getContent(), "InlineResponse" + response.getKey(), globalObject))
                                          .build());
      }
    }
    return responseObjects;
  }

  private static List<ContentObject> mapContentObject(FileSpec fileSpec, Content content, String inlineObject, GlobalObject globalObject) {
    List<ContentObject> contentObjects = new ArrayList<>();
    if (Objects.nonNull(content)) {
      for (Entry<String, MediaType> mediaTypeEntry : content.entrySet()) {
        if (Objects.nonNull(mediaTypeEntry.getValue().getSchema().getProperties())) {
          contentObjects.add(ContentObject.builder()
                                          .typeData(mapDataType(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .name(mediaTypeEntry.getKey())
                                          .description(mediaTypeEntry.getValue().getSchema().getDescription())
                                          .importName(getPojoName(inlineObject, fileSpec))
                                          .refNameObject(mapRefNameObject(getPojoName(inlineObject, fileSpec),true))
                                          .build());
        } else if (Objects.nonNull(mediaTypeEntry.getValue().getSchema().getType()) &&
                   BasicTypeConstants.BASIC_OBJECT_TYPE.contains(mediaTypeEntry.getValue().getSchema().getType())) {
          contentObjects.add(ContentObject.builder()
                                          .typeData(mapDataType(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .name(mediaTypeEntry.getKey())
                                          .description(mediaTypeEntry.getValue().getSchema().getDescription())
                                          .refNameObject(mapRefNameObject(defineTypeName(mediaTypeEntry.getValue().getSchema()), false))
                                          .build());
        } else {
          contentObjects.add(ContentObject.builder()
                                          .typeData(mapDataType(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .name(mediaTypeEntry.getKey())
                                          .description(mediaTypeEntry.getValue().getSchema().getDescription())
                                          .importName(mapRefName(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .refNameObject(mapRefNameObject(mapRefName(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()), true))
                                          .build());
        }
      }
    }
    return contentObjects;
  }

  private static RefNameObject mapRefNameObject(String refName, Boolean checkImport) {
    return RefNameObject.builder()
                        .refName(refName)
                        .checkImport(checkImport)
                        .build();

  }

  private static String defineTypeName(Schema schema) {
    String typeName = "";
    switch (schema.getType()) {
      case "integer":
        if ("int32".equalsIgnoreCase(schema.getFormat()) || !Objects.nonNull(schema.getFormat())) {
          typeName = "Integer";
        } else if ("int64".equalsIgnoreCase(schema.getFormat())) {
          typeName = "Long";
        }
        break;
      case "number":
        if ("float".equalsIgnoreCase(schema.getFormat())) {
          typeName = "Float";
        } else if ("double".equalsIgnoreCase(schema.getFormat())) {
          typeName = "Double";
        } else if (schema.getFormat().isEmpty()) {
          typeName = "Integer";
        }
        break;
      case "boolean":
        typeName = "Boolean";
        break;
      case "string":
      default:
        typeName = "String";
        break;
    }
    return typeName;
  }

  private static String mapDataType(Schema schema, Map<String, String> componentsTypes) {
    var type = getSchemaType(schema, componentsTypes);
    return StringUtils.isNotBlank(type) ? type : "";
  }

  private static String getSchemaType(Schema schema, Map<String, String> componentsTypes) {
    String dataType = schema.getType();

    if (!StringUtils.isNotBlank(dataType) && Objects.nonNull(schema.get$ref())) {
      String[] wholeRef = schema.get$ref().split("/");
      dataType = componentsTypes.getOrDefault(wholeRef[wholeRef.length - 1], "");
    }
    return dataType.startsWith("array") ? "array" : dataType.startsWith("map") ? "map" : dataType;
  }

  private static String mapRefName(Schema schema, Map<String, String> componentsTypes) {

    var refSchema = "";

    if ("array".equalsIgnoreCase(schema.getType())) {
      ArraySchema arraySchema = (ArraySchema) schema;
      refSchema = arraySchema.getItems().get$ref();

    }

    if (Objects.nonNull(schema.get$ref())) {
      refSchema = schema.get$ref();
    }

    return StringUtils.isNotBlank(refSchema) ? getRefSchema(refSchema, componentsTypes) : "";
  }

  private static String getRefSchema(String refSchema, Map<String, String> componentsTypes) {
    String[] wholeRef = refSchema.split("/");
    var refName = componentsTypes.getOrDefault(wholeRef[wholeRef.length - 1], "");

    if (StringUtils.isNotBlank(refName) && refName.contains("-")) {
      String[] wholeRefName = refName.split("-");
      refName = wholeRefName[wholeRefName.length - 1];
    }

    return refName;
  }

  private static Boolean checkIfOperationIsNull(Operation operation) {
    return Objects.nonNull(operation);
  }

  private static List<String> getSecurityRequirementList(
    List<SecurityRequirement> securityRequirementList,
    List<String> authentications) {
    var authSecList = new ArrayList<String>();
    if (null != securityRequirementList
        && !securityRequirementList.isEmpty()) {
      securityRequirementList.forEach(securityRequirement -> securityRequirement.forEach((key, value) -> authSecList.add(key)));
    } else {
      return authentications;
    }
    return authSecList;
  }

}
