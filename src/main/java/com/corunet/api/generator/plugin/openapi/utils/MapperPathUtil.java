/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.corunet.api.generator.plugin.openapi.model.AuthSchemaObject;
import com.corunet.api.generator.plugin.openapi.model.BasicTypeConstants;
import com.corunet.api.generator.plugin.openapi.model.ContentObject;
import com.corunet.api.generator.plugin.openapi.model.GlobalObject;
import com.corunet.api.generator.plugin.openapi.model.OperationObject;
import com.corunet.api.generator.plugin.openapi.model.ParameterObject;
import com.corunet.api.generator.plugin.openapi.model.PathObject;
import com.corunet.api.generator.plugin.openapi.model.RequestObject;
import com.corunet.api.generator.plugin.openapi.model.ResponseObject;
import com.corunet.api.generator.plugin.openapi.parameter.FileSpec;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
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
      PathObject pathObject = PathObject.builder()
                                        .pathName(pathItem.getKey())
                                        .globalObjects(globalObject)
                                        .operationObject(mapOperationObject(fileSpec, pathItem, globalObject))
                                        .build();

      pathObjects.add(pathObject);
    }

    return pathObjects;
  }

  private static List<OperationObject> mapOperationObject(FileSpec fileSpec, Entry<String, PathItem> path,  GlobalObject globalObject){
    ArrayList<OperationObject> operationObjects = new ArrayList<>();
    if (checkIfOperationIsNull(path.getValue().getGet())) {
      OperationObject operationObject = OperationObject.builder()
                                                       .operationId(path.getValue().getGet().getOperationId())
                                                       .operationType("GET")
                                                       .summary(path.getValue().getGet().getSummary())
                                                       .tags(path.getValue().getGet().getTags())
                                                       .requestObjects(mapRequestObject(fileSpec,path.getValue().getGet(), globalObject))
                                                       .responseObjects(mapResponseObject(fileSpec,path.getValue().getGet().getResponses(), globalObject))
                                                       .parameterObjects(mapParameterObjects(path.getValue().getGet()))
                                                       .security(getSecurityRequirementList(path.getValue().getGet().getSecurity(), globalObject.getAuthentications()))
                                                       .consumes(getConsumesList(path.getValue().getGet().getRequestBody()))
                                                       .produces(getProducesList(path.getValue().getGet().getResponses()))
                                                       .build();
      operationObjects.add(operationObject);
    }
    if (checkIfOperationIsNull(path.getValue().getPost())) {
      OperationObject operationObject = OperationObject.builder()
                                                       .operationId(path.getValue().getPost().getOperationId())
                                                       .operationType("POST")
                                                       .summary(path.getValue().getPost().getSummary())
                                                       .tags(path.getValue().getPost().getTags())
                                                       .requestObjects(mapRequestObject(fileSpec,path.getValue().getPost(), globalObject))
                                                       .responseObjects(mapResponseObject(fileSpec,path.getValue().getPost().getResponses(), globalObject))
                                                       .parameterObjects(mapParameterObjects(path.getValue().getPost()))
                                                       .security(getSecurityRequirementList(path.getValue().getPost().getSecurity(), globalObject.getAuthentications()))
                                                       .consumes(getConsumesList(path.getValue().getPost().getRequestBody()))
                                                       .produces(getProducesList(path.getValue().getPost().getResponses()))
                                                       .build();
      operationObjects.add(operationObject);
    }
    if (checkIfOperationIsNull(path.getValue().getDelete())) {
      OperationObject operationObject = OperationObject.builder()
                                                       .operationId(path.getValue().getDelete().getOperationId())
                                                       .operationType("DELETE")
                                                       .summary(path.getValue().getDelete().getSummary())
                                                       .tags(path.getValue().getDelete().getTags())
                                                       .requestObjects(mapRequestObject(fileSpec,path.getValue().getDelete(), globalObject))
                                                       .responseObjects(mapResponseObject(fileSpec,path.getValue().getDelete().getResponses(), globalObject))
                                                       .parameterObjects(mapParameterObjects(path.getValue().getDelete()))
                                                       .security(getSecurityRequirementList(path.getValue().getDelete().getSecurity(), globalObject.getAuthentications()))
                                                       .consumes(getConsumesList(path.getValue().getDelete().getRequestBody()))
                                                       .produces(getProducesList(path.getValue().getDelete().getResponses()))
                                                       .build();
      operationObjects.add(operationObject);
    }
    if (checkIfOperationIsNull(path.getValue().getPut())) {
      OperationObject operationObject = OperationObject.builder()
                                                       .operationId(path.getValue().getPut().getOperationId())
                                                       .operationType("PUT")
                                                       .summary(path.getValue().getPut().getSummary())
                                                       .tags(path.getValue().getPut().getTags())
                                                       .requestObjects(mapRequestObject(fileSpec,path.getValue().getPut(), globalObject))
                                                       .responseObjects(mapResponseObject(fileSpec,path.getValue().getPut().getResponses(), globalObject))
                                                       .parameterObjects(mapParameterObjects(path.getValue().getPut()))
                                                       .security(getSecurityRequirementList(path.getValue().getPut().getSecurity(), globalObject.getAuthentications()))
                                                       .consumes(getConsumesList(path.getValue().getPut().getRequestBody()))
                                                       .produces(getProducesList(path.getValue().getPut().getResponses()))
                                                       .build();
      operationObjects.add(operationObject);
    }
    if (checkIfOperationIsNull(path.getValue().getPatch())) {
      OperationObject operationObject = OperationObject.builder()
                                                       .operationId(path.getValue().getPatch().getOperationId())
                                                       .operationType("PATCH")
                                                       .summary(path.getValue().getPatch().getSummary())
                                                       .tags(path.getValue().getPatch().getTags())
                                                       .requestObjects(mapRequestObject(fileSpec,path.getValue().getPatch(), globalObject))
                                                       .responseObjects(mapResponseObject(fileSpec,path.getValue().getPatch().getResponses(), globalObject))
                                                       .parameterObjects(mapParameterObjects(path.getValue().getPatch()))
                                                       .security(getSecurityRequirementList(path.getValue().getPatch().getSecurity(), globalObject.getAuthentications()))
                                                       .consumes(getConsumesList(path.getValue().getPatch().getRequestBody()))
                                                       .produces(getProducesList(path.getValue().getPatch().getResponses()))
                                                       .build();
      operationObjects.add(operationObject);
    }

    return operationObjects;
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


  private static List<RequestObject> mapRequestObject(FileSpec fileSpec, Operation operation, GlobalObject globalObject)  {
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

  private static List<ParameterObject> mapParameterObjects(Operation operation) {
    List<ParameterObject> parameterObjects = new ArrayList<>();
    if (Objects.nonNull(operation.getParameters())) {
      operation.getParameters().forEach(parameter -> parameterObjects.add(ParameterObject.builder()
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

  private static List<ContentObject> mapContentObject(FileSpec fileSpec, Content content, String inlineObject, GlobalObject globalObject){
    List<ContentObject> contentObjects = new ArrayList<>();
    if (Objects.nonNull(content)) {
      for (Entry<String, MediaType> mediaTypeEntry : content.entrySet()) {
        if (Objects.nonNull(mediaTypeEntry.getValue().getSchema().getProperties())) {
          contentObjects.add(ContentObject.builder()
                                          .typeData(mapDataType(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .name(mediaTypeEntry.getKey())
                                          .description(mediaTypeEntry.getValue().getSchema().getDescription())
                                          .importName(getPojoName(inlineObject, fileSpec))
                                          .refName(getPojoName(inlineObject, fileSpec))
                                          .build());
        } else if(Objects.nonNull(mediaTypeEntry.getValue().getSchema().getType()) && BasicTypeConstants.BASIC_OBJECT_TYPE.contains(mediaTypeEntry.getValue().getSchema().getType())) {
          contentObjects.add(ContentObject.builder()
                                          .typeData(mapDataType(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .name(mediaTypeEntry.getKey())
                                          .description(mediaTypeEntry.getValue().getSchema().getDescription())
                                          .refName(defineTypeName(mediaTypeEntry.getValue().getSchema()))
                                          .build());
        } else{
          contentObjects.add(ContentObject.builder()
                                          .typeData(mapDataType(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .name(mediaTypeEntry.getKey())
                                          .description(mediaTypeEntry.getValue().getSchema().getDescription())
                                          .importName(mapRefName(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .refName(mapRefName(mediaTypeEntry.getValue().getSchema(), globalObject.getComponentsTypeMap()))
                                          .build());
        }
      }
    }
    return contentObjects;
  }

  private static String defineTypeName(Schema schema) {
    String typeName = "";
    switch (schema.getType()) {
      case "string":
        typeName = "String";
        break;
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

  private static String getTypeMap(MapSchema mapSchema, FileSpec fileSpec) {
    var typeMap = "";
    if (mapSchema.getAdditionalProperties() instanceof StringSchema) {
      typeMap = "String";
    } else if (mapSchema.getAdditionalProperties() instanceof IntegerSchema) {
      typeMap = "Integer";
    } else {
      Schema schema = (Schema) mapSchema.getAdditionalProperties();
      if (StringUtils.isNotBlank(schema.get$ref())) {
        String[] pathObjectRef = schema.get$ref().split("/");
        typeMap = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
      }
    }
    return typeMap;
  }

  private static String getTypeArray(ArraySchema array, FileSpec fileSpec) {
    var typeArray = "";
    if (array.getItems() instanceof StringSchema) {
      typeArray = "String";
    } else if (array.getItems() instanceof IntegerSchema) {
      typeArray = "Integer";
    } else if (StringUtils.isNotBlank(array.getItems().get$ref())) {
      String[] pathObjectRef = array.getItems().get$ref().split("/");
      typeArray = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
    }
    return typeArray;
  }

  private static String getPojoName(String namePojo, FileSpec fileSpec) {
    return (StringUtils.isNotBlank(fileSpec.getModelNamePrefix()) ? fileSpec.getModelNamePrefix() : "")
           + namePojo
           + (StringUtils.isNotBlank(fileSpec.getModelNameSuffix()) ? fileSpec.getModelNameSuffix() : "");
  }

  private static List<String> getSecurityRequirementList(List<SecurityRequirement> securityRequirementList,
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

  private static String getSimpleType(Schema schema) {
    String type = "";
    if ("number".equalsIgnoreCase(schema.getType())) {
      if ("float".equalsIgnoreCase(schema.getFormat())) {
        type = "float";
      } else if ("double".equalsIgnoreCase(schema.getFormat())) {
        type = "double";
      } else {
        type = "integer";
      }
    } else if ("integer".equalsIgnoreCase(schema.getType())) {
      if ("int64".equalsIgnoreCase(schema.getType())) {
        type = "long";
      } else {
        type = "integer";
      }
    } else {
      type = schema.getType();
    }
    return type;
  }

}
