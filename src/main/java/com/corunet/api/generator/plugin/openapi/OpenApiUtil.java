package com.corunet.api.generator.plugin.openapi;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.corunet.api.generator.plugin.openapi.model.AuthObject;
import com.corunet.api.generator.plugin.openapi.model.AuthSchemaObject;
import com.corunet.api.generator.plugin.openapi.model.ContentObject;
import com.corunet.api.generator.plugin.openapi.model.GlobalObject;
import com.corunet.api.generator.plugin.openapi.model.OperationObject;
import com.corunet.api.generator.plugin.openapi.model.ParameterObject;
import com.corunet.api.generator.plugin.openapi.model.PathObject;
import com.corunet.api.generator.plugin.openapi.model.RequestObject;
import com.corunet.api.generator.plugin.openapi.model.ResponseObject;
import com.corunet.api.generator.plugin.openapi.model.SchemaFieldObject;
import com.corunet.api.generator.plugin.openapi.model.SchemaObject;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.PathItem.HttpMethod;
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
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.MojoExecutionException;

public class OpenApiUtil {

  public static HashMap<String, HashMap<String, PathItem>> mapApiGroups(OpenAPI openAPI, Boolean groupByTags) {
    var mapApis = new HashMap<String, HashMap<String, PathItem>>();

    if (!openAPI.getPaths().isEmpty()) {
      mapApis = null != groupByTags && groupByTags ? mapApiGroupsByTags(openAPI) : mapApiGroupsByUrl(openAPI);
    }

    return mapApis;
  }

  public static GlobalObject mapOpenApiObjectToOurModels(OpenAPI openAPI, FileSpec fileSpec) {

    var authSchemaList = createAuthSchemaList(openAPI);
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

  private static List<AuthSchemaObject> createAuthSchemaList(OpenAPI openAPI) {
    ArrayList<AuthSchemaObject> authList = new ArrayList<>();
    if (null != openAPI.getComponents().getSecuritySchemes()
        && !openAPI.getComponents().getSecuritySchemes().isEmpty()) {

      openAPI.getComponents().getSecuritySchemes().forEach((key, value) -> {
        var authSchema = AuthSchemaObject.builder()
                                         .type(value.getType().toString().equalsIgnoreCase("http")
                                               && value.getScheme().equalsIgnoreCase("bearer") ? "HttpBearerAuth" : getModelTypeAuth(value))
                                         .name(key)
                                         .apiKeyParam(value.getType().toString().equalsIgnoreCase("apiKey")
                                                          ? value.getName() : "")
                                         .apiKeyPlace(value.getType().toString().equalsIgnoreCase("apiKey")
                                                          ? value.getIn().toString() : "")
                                         .bearerSchema(value.getType().toString().equalsIgnoreCase("http")
                                                       && value.getScheme().equalsIgnoreCase("bearer") ? value.getScheme() : "")
                                         .build();

        authList.add(authSchema);
      });

    }

    return authList;
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

  private static String getModelTypeAuth(SecurityScheme securityScheme) {
    var type = securityScheme.getType().toString();
    if (securityScheme.getType().toString().equalsIgnoreCase("apiKey")) {
      type = "ApiKeyAuth";
    } else if (securityScheme.getType().toString().equalsIgnoreCase("oauth2")) {
      type = "OAuth";
    } else if (securityScheme.getType().toString().equalsIgnoreCase("http")) {
      type = "HttpBasicAuth";
    }

    return type;
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
                                                                                         .className(OpenApiUtil.getSimpleType(parameter.getSchema()))
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

  public static HashMap<String, String> getMapComponentsTypes(Components components, FileSpec fileSpec) {
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

  public static SchemaObject mapComponentToSchemaObject(
      Schema schema, String nameSchema,
      FileSpec fileSpec, String modelPackage) {
    var listSchema = getFields(schema, fileSpec);

    return SchemaObject.builder()
                       .description(schema.getDescription())
                       .schemaName(schema.getName())
                       .className(getPojoName(nameSchema, fileSpec))
                       .importList(getImportList(listSchema, modelPackage))
                       .fieldObjectList(listSchema)
                       .build();
  }

  private static String getPojoName(String namePojo, FileSpec fileSpec) {
    return (StringUtils.isNotBlank(fileSpec.getModelNamePrefix()) ? fileSpec.getModelNamePrefix() : "")
           + namePojo
           + (StringUtils.isNotBlank(fileSpec.getModelNameSuffix()) ? fileSpec.getModelNameSuffix() : "");
  }

  private static List<String> getImportList(
      List<SchemaFieldObject> fieldObjectList,
      String modelPackage) {
    var listHashMap = new HashMap<String, List<String>>();
    var importList = new ArrayList<String>();

    fieldObjectList.forEach(fieldObject -> {
      if (fieldObject.getDataTypeSimple().equals("array") && !listHashMap.containsKey("array")) {
        var arrayImport = new ArrayList<String>();
        arrayImport.add("java.util.List");
        arrayImport.add("java.util.ArrayList");
        listHashMap.put("array", arrayImport);
      }

      if (Objects.equals(fieldObject.getDataTypeSimple(), "map") && !listHashMap.containsKey("map")) {
        var arrayImport = new ArrayList<String>();
        arrayImport.add("java.util.Map");
        arrayImport.add("java.util.HashMap");
        listHashMap.put("map", arrayImport);
      }

      if (StringUtils.isNotBlank(fieldObject.getImportClass())
          && !listHashMap.containsKey(fieldObject.getImportClass())) {
        var arrayImport = new ArrayList<String>();
        arrayImport.add(modelPackage + "." + fieldObject.getImportClass());
        listHashMap.put(fieldObject.getImportClass(), arrayImport);
      }

    });

    if (!listHashMap.isEmpty()) {
      listHashMap.forEach((key, value) -> importList.addAll(value));
    }

    return importList;
  }

  private static List<SchemaFieldObject> getFields(Schema schema, FileSpec fileSpec) {
    var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    var mapperProperties = new HashMap<String, Schema>(schema.getProperties());

    mapperProperties.forEach((key, value) -> {
      var field = SchemaFieldObject.builder()
                                   .baseName(key)
                                   .dataTypeSimple(getSimpleType(value))
                                   .build();
      if (value instanceof ArraySchema) {
        var typeArray = getTypeArray((ArraySchema) value, fileSpec);
        field.setDataType(typeArray);
        field.setImportClass(getImportClass(typeArray));
      } else if (value instanceof MapSchema) {
        var typeMap = getTypeMap((MapSchema) value, fileSpec);
        field.setDataTypeSimple("map");
        field.setDataType(typeMap);
        field.setImportClass(getImportClass(typeMap));
      } else if (value.getType().equals("object")) {
        var typeObject = "";
        if (StringUtils.isNotBlank(value.get$ref())) {
          String[] pathObjectRef = schema.get$ref().split("/");
          typeObject = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
        }
        field.setImportClass(getImportClass(typeObject));
        field.setDataType(typeObject);
      }

      fieldObjectArrayList.add(field);

    });
    return fieldObjectArrayList;
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

  private static String getImportClass(String type) {
    return StringUtils.isNotBlank(type) && (!type.equals("String") && !type.equals("Integer")) ? type : "";
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

  private static List<String> getApiAuthNames(List<PathObject> pathObjects) {
    var operationList = new ArrayList<OperationObject>();
    pathObjects.forEach(pathObject -> operationList.addAll(pathObject.getOperationObject()));
    return addApiAuthNames(operationList);
  }

  private static List<String> addApiAuthNames(List<OperationObject> operationList) {
    var authList = new ArrayList<String>();

    operationList.forEach(operationObject -> {
      if (null != operationObject.getSecurity() && !operationObject.getSecurity().isEmpty()) {
        operationObject.getSecurity().forEach(auth -> {
          if (!authList.contains(auth)) {
            authList.add(auth);
          }
        });
      }
    });

    return authList;
  }

  public static AuthObject getApiAuthObject(List<AuthSchemaObject> authSchemas, List<PathObject> pathObjects) {
    var authList = getApiAuthNames(pathObjects);
    var authApiList = new ArrayList<String>();
    if (null != authSchemas && !authSchemas.isEmpty() && !authList.isEmpty()) {
      authSchemas.forEach(authValue -> {
        if (authList.contains(authValue.getName()) && !authApiList.contains(authValue.getType())) {
          authApiList.add(authValue.getType());
        }
      });
    }
    return AuthObject.builder().securityRequirements(authApiList).build();
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
      operation.getRequestBody().getContent().entrySet().forEach(content -> basicSchemaMap.put("InlineObject" + operationId,
                                                                                               content.getValue().getSchema()));
    }
    for (Entry<String, ApiResponse> response : operation.getResponses().entrySet()) {
      if (Objects.nonNull(response.getValue().getContent())) {
        response.getValue().getContent().entrySet().forEach(content -> basicSchemaMap.put("InlineResponse" + response.getKey(), content.getValue().getSchema()));
      }
    }
  }
}
