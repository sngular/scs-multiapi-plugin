/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import static com.sngular.api.generator.plugin.common.tools.ApiTool.REF;
import static com.sngular.api.generator.plugin.common.tools.ApiTool.SCHEMAS;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.PluginConstants;
import com.sngular.api.generator.plugin.asyncapi.util.ReferenceProcessor;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import com.sngular.api.generator.plugin.common.parameter.OperationParameter;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.util.GeneratorUtil;
import com.sngular.api.generator.plugin.exception.GeneratedSourcesException;
import com.sngular.api.generator.plugin.exception.GeneratorTemplateException;
import com.sngular.api.generator.plugin.openapi.exception.CodeGenerationException;
import com.sngular.api.generator.plugin.openapi.exception.DuplicateModelClassException;
import com.sngular.api.generator.plugin.openapi.model.AuthObject;
import com.sngular.api.generator.plugin.openapi.model.GlobalObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import com.sngular.api.generator.plugin.openapi.model.SchemaObject;
import com.sngular.api.generator.plugin.openapi.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.parameter.OpenAPIOperationParameter;
import com.sngular.api.generator.plugin.openapi.parameter.OpenAPISpecFile;
import com.sngular.api.generator.plugin.openapi.template.TemplateFactory;
import com.sngular.api.generator.plugin.openapi.template.TemplateIndexConstants;
import com.sngular.api.generator.plugin.openapi.utils.MapperAuthUtil;
import com.sngular.api.generator.plugin.openapi.utils.MapperContentUtil;
import com.sngular.api.generator.plugin.openapi.utils.MapperPathUtil;
import com.sngular.api.generator.plugin.openapi.utils.MapperUtil;
import com.sngular.api.generator.plugin.openapi.utils.OpenApiUtil;
import freemarker.template.TemplateException;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

public class OpenApiGenerator {

  private static final String SLASH = "/";
  private static final String DEFAULT_OPENAPI_API_PACKAGE =
      PluginConstants.DEFAULT_API_PACKAGE + ".openapi";

  private static final String DEFAULT_OPENAPI_MODEL_PACKAGE =
      DEFAULT_OPENAPI_API_PACKAGE + ".model";

  private static final String DEFAULT_OPENAPI_CLIENT_PACKAGE =
      DEFAULT_OPENAPI_API_PACKAGE + ".client";

  private static final Pattern PACKAGE_SEPARATOR = Pattern.compile("\\.");

  private final Boolean overwriteModel;

  private boolean generateExceptionTemplate;

  private final FilenameFilter targetFileFilter;

  private final Set<String> overwriteModelList = new HashSet<>();

  private final TemplateFactory templateFactory;

  private final String processedGeneratedSourcesFolder;

  private final String groupId;

  private final File targetFolder;

  private final Path baseDir;

  private Boolean isWebClient = false;

  private Boolean isRestClient = false;

  private final List<String> authentications = new ArrayList<>();

  private boolean useLombok;

  private final Integer springBootVersion;

  public OpenApiGenerator(
      final Integer springBootVersion,
      final Boolean overwriteModel,
      final String processedGeneratedSourcesFolder,
      final String groupId,
      final File targetFolder,
      final File basedir) {
    templateFactory = new TemplateFactory();
    this.overwriteModel = overwriteModel;
    this.processedGeneratedSourcesFolder = processedGeneratedSourcesFolder;
    this.groupId = groupId;
    this.targetFolder = targetFolder;
    this.baseDir = basedir.toPath().toAbsolutePath();
    this.targetFileFilter =
        (dir, name) -> name.toLowerCase().contains(targetFolder.toPath().getFileName().toString());
    this.springBootVersion = springBootVersion;
  }

  public final void processFileSpec(final List<OpenAPISpecFile> specsListFile) {
    final ObjectMapper om = new ObjectMapper(new YAMLFactory());
    for (OpenAPISpecFile specFile : specsListFile) {
      OpenAPIOperationParameter openAPIOperationParameter = specFile.getOpenApiOperationParameter();
      generateExceptionTemplate = false;
      useLombok = Boolean.TRUE.equals(openAPIOperationParameter.isUseLombokModelAnnotation());
      try {
        String filePath = specFile.getOpenApiOperationParameter().getFilePath();
        final Pair<InputStream, FileLocation> ymlFileAndPath =
            GeneratorUtil.resolveYmlLocation(filePath, this.getClass());
        final InputStream ymlFile = ymlFileAndPath.getLeft();
        final FileLocation ymlParent = ymlFileAndPath.getRight();
        final JsonNode openApi = om.readTree(ymlFile);
        final Map<String, JsonNode> totalSchemas = getAllSchemas(ymlParent, openApi);

        setUpTemplate(openAPIOperationParameter);
        final String filePathToSave = processPath(openAPIOperationParameter.getApiPackage(), false);
        processFile(openAPIOperationParameter, filePathToSave, totalSchemas);
        createClients(openAPIOperationParameter);
      } catch (final IOException e) {
        throw new CodeGenerationException(
            "Code generation failed. See above for the full exception.", e);
      }
    }
  }

  private Map<String, JsonNode> getAllSchemas(final FileLocation ymlParent, final JsonNode node) {
    final Map<String, JsonNode> totalSchemas = new HashMap<>();
    final List<JsonNode> referenceList = node.findValues(REF);

    referenceList.forEach(
        reference -> {
          final ReferenceProcessor refProcessor =
              ReferenceProcessor.builder().ymlParent(ymlParent).totalSchemas(totalSchemas).build();
          refProcessor.processReference(node, ApiTool.getNodeAsString(reference));
        });

    ApiTool.getComponent(node, SCHEMAS)
        .forEachRemaining(schema -> totalSchemas.putIfAbsent(schema.getKey(), schema.getValue()));

    return totalSchemas;
  }

  private final void setUpTemplate(OpenAPIOperationParameter operationParameter) {
    processPackage(operationParameter.getApiPackage());
  }

  private void processFile(
      final OpenAPIOperationParameter operationParameter,
      String filePathToSave,
      Map<String, JsonNode> totalSchemas)
      throws IOException {

    final JsonNode openAPI = OpenApiUtil.getPojoFromSpecFile(baseDir, operationParameter);
    final String clientPackage = operationParameter.getClientPackage();

    if (operationParameter.isCallMode()) {
      templateFactory.setWebClientPackageName(
          StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE);
      templateFactory.setAuthPackageName(
          (StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE)
              + ".auth");
      isWebClient = operationParameter.isReactive();
      isRestClient = !operationParameter.isReactive();
    }

    templateFactory.calculateJavaEEPackage(springBootVersion);
    final var globalObject = createApiTemplate(operationParameter, filePathToSave, openAPI);

    createModelTemplate(operationParameter, totalSchemas);
  }

  private void createClients(final OpenAPIOperationParameter operationParameter) {

    if (isWebClient || isRestClient) {
      try {
        final String clientPackage = operationParameter.getClientPackage();
        final String clientPath =
            processPath(
                StringUtils.isNotBlank(clientPackage)
                    ? clientPackage
                    : DEFAULT_OPENAPI_CLIENT_PACKAGE,
                false);
        if (Boolean.TRUE.equals(isWebClient)) {
          templateFactory.fillTemplateWebClient(clientPath);
        }
        if (Boolean.TRUE.equals(isRestClient)) {
          templateFactory.fillTemplateRestClient(clientPath);
        }
        createAuthTemplates(operationParameter);
      } catch (IOException | TemplateException e) {
        throw new GeneratorTemplateException("Template Generator problem", e);
      }
    }
  }

  private void createAuthTemplates(final OpenAPIOperationParameter operationParameter)
      throws TemplateException, IOException {
    final String clientPackage = operationParameter.getClientPackage();
    final var authFileRoot =
        (StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE)
            + ".auth";
    final String authFileToSave = processPath(authFileRoot, false);

    templateFactory.setAuthPackageName(authFileRoot);
    templateFactory.fillTemplateAuth(authFileToSave, "Authentication");

    if (!authentications.isEmpty()) {
      for (String authentication : authentications) {
        templateFactory.fillTemplateAuth(authFileToSave, authentication);
      }
    }
  }

  private GlobalObject createApiTemplate(
      final OpenAPIOperationParameter operationParameter,
      final String filePathToSave,
      final JsonNode openAPI) {
    final MultiValuedMap<String, Map<String, JsonNode>> apis =
        OpenApiUtil.mapApiGroups(openAPI, operationParameter.isUseTagsGroup());
    final var authSchemaList = MapperAuthUtil.createAuthSchemaList(openAPI);
    final GlobalObject globalObject =
        MapperPathUtil.mapOpenApiObjectToOurModels(openAPI, authSchemaList);

    for (var apisKey : apis.keySet()) {
      final String javaFileName = OpenApiUtil.processJavaFileName(apisKey);
      final List<PathObject> pathObjects =
          MapperPathUtil.mapPathObjects(
              operationParameter, apis.get(apisKey), globalObject, baseDir);
      final AuthObject authObject =
          MapperAuthUtil.getApiAuthObject(globalObject.getAuthSchemas(), pathObjects);

      try {
        templateFactory.fillTemplate(
            filePathToSave, operationParameter, javaFileName, pathObjects, authObject);
      } catch (IOException | TemplateException e) {
        throw new GeneratorTemplateException(
            "Error filling the template", operationParameter.getFilePath(), e);
      }

      if (Boolean.TRUE.equals(operationParameter.isCallMode())) {
        addAuthentications(authObject);
      }
    }

    return globalObject;
  }

  private void addAuthentications(final AuthObject authObject) {

    if (null != authObject.getSecurityRequirements()
        && !authObject.getSecurityRequirements().isEmpty()) {
      authObject
          .getSecurityRequirements()
          .forEach(
              authType -> {
                if (!authentications.contains(authType)) {
                  authentications.add(authType);
                }
              });
    }
  }

  private void createModelTemplate(
      final OperationParameter operationParameter, final Map<String, JsonNode> totalSchemas)
      throws IOException {
    final String fileModelToSave = processPath(operationParameter.getModelPackage(), true);
    final var modelPackage = processModelPackage(operationParameter.getModelPackage());
    templateFactory.setModelPackageName(modelPackage);
    processModels(
        operationParameter,
        fileModelToSave,
        modelPackage,
        totalSchemas,
        Boolean.TRUE.equals(overwriteModel));
  }

  private void processPackage(final String apiPackage) {
    if (StringUtils.isNotBlank(apiPackage)) {
      templateFactory.setPackageName(apiPackage.trim());
    } else {
      templateFactory.setPackageName(
          Objects.requireNonNullElse(groupId, DEFAULT_OPENAPI_API_PACKAGE));
    }
  }

  private String processModelPackage(final String modelPackage) {
    var modelReturnPackage = "";
    if (StringUtils.isNotBlank(modelPackage)) {
      modelReturnPackage = modelPackage.trim();
    } else if (groupId != null) {
      modelReturnPackage = groupId + ".model";
    } else {
      modelReturnPackage = DEFAULT_OPENAPI_MODEL_PACKAGE;
    }
    return modelReturnPackage;
  }

  private String processPath(final String fileSpecPackage, final boolean isModel)
      throws IOException {
    Path path;
    final File[] pathList = Objects.requireNonNull(baseDir.toFile().listFiles(targetFileFilter));
    if (pathList.length > 0) {
      path = pathList[0].toPath().resolve(convertPackageToTargetPath(fileSpecPackage, isModel));
    } else {
      path = targetFolder.toPath();
      if (path.toFile().exists() || path.toFile().mkdirs()) {
        path = path.resolve(convertPackageToTargetPath(fileSpecPackage, isModel));
      } else {
        throw new IOException("Problem creating folders: " + path.toFile());
      }
    }
    if (!path.toFile().isDirectory() && !path.toFile().mkdirs()) {
      throw new IOException("Problem creating folders: " + path.toFile());
    }
    return path.toString();
  }

  private String convertPackageToTargetPath(final String fileSpecPackage, final boolean isModel) {
    final String toMatch =
        StringUtils.defaultIfBlank(
            fileSpecPackage,
            StringUtils.defaultIfBlank(
                groupId, isModel ? DEFAULT_OPENAPI_MODEL_PACKAGE : DEFAULT_OPENAPI_API_PACKAGE));
    return FilenameUtils.concat(
        processedGeneratedSourcesFolder, PACKAGE_SEPARATOR.matcher(toMatch).replaceAll("/"));
  }

  private void processModels(
      final OperationParameter specFile,
      final String fileModelToSave,
      final String modelPackage,
      final Map<String, JsonNode> totalSchemas,
      final boolean overwrite) {
    final Map<String, SchemaObject> builtSchemasMap = new HashMap<>();
    totalSchemas.forEach(
        (schemaName, basicSchema) ->
            processModel(
                specFile,
                fileModelToSave,
                modelPackage,
                totalSchemas,
                overwrite,
                schemaName,
                basicSchema,
                builtSchemasMap));
  }

  private void processModel(
      final OperationParameter specFile,
      final String fileModelToSave,
      final String modelPackage,
      final Map<String, JsonNode> totalSchemas,
      final boolean overwrite,
      final String schemaName,
      final JsonNode basicSchema,
      final Map<String, SchemaObject> builtSchemasMap) {
    if (!overwrite && !overwriteModelList.add(schemaName + modelPackage)) {
      throw new DuplicateModelClassException(schemaName, modelPackage);
    }

    if (ApiTool.hasRef(basicSchema)) {
      final var refSchema = MapperUtil.getRefSchemaName(basicSchema);
      builtSchemasMap.putAll(
          writeModel(
              specFile,
              fileModelToSave,
              refSchema,
              totalSchemas.get(refSchema),
              totalSchemas,
              builtSchemasMap));
    } else if (!ApiTool.isArray(basicSchema)
        && !TypeConstants.STRING.equalsIgnoreCase(ApiTool.getType(basicSchema))) {
      builtSchemasMap.putAll(
          writeModel(
              specFile, fileModelToSave, schemaName, basicSchema, totalSchemas, builtSchemasMap));
    }
  }

  private Map<String, SchemaObject> writeModel(
      final OperationParameter operationParameter,
      final String fileModelToSave,
      final String schemaName,
      final JsonNode basicSchema,
      final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> builtSchemasMap) {
    final var schemaObjectMap =
        MapperContentUtil.mapComponentToSchemaObject(
            totalSchemas, builtSchemasMap, schemaName, basicSchema, operationParameter);
    checkRequiredOrCombinatorExists(schemaObjectMap);
    schemaObjectMap
        .values()
        .forEach(
            schemaObject -> {
              try {
                final Set<String> propertiesSet = new HashSet<>();
                templateFactory.fillTemplateSchema(
                    fileModelToSave,
                    operationParameter.isUseLombokModelAnnotation(),
                    schemaObject,
                    propertiesSet);
                fillTemplates(fileModelToSave, propertiesSet);
              } catch (IOException | TemplateException e) {
                throw new GeneratedSourcesException(schemaObject.getClassName(), e);
              }
            });

    if (Boolean.TRUE.equals(generateExceptionTemplate)) {
      try {
        templateFactory.fillTemplateModelClassException(fileModelToSave, true);
      } catch (IOException | TemplateException e) {
        throw new GeneratedSourcesException(fileModelToSave, e);
      }
    }
    return schemaObjectMap;
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private void fillTemplates(final String filePathToSave, final Set<String> fieldProperties)
      throws TemplateException, IOException {
    for (final String current : fieldProperties) {
      switch (current) {
        case "Size":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "Size.java",
              "SizeValidator.java",
              TemplateIndexConstants.TEMPLATE_SIZE_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_SIZE_VALIDATOR_ANNOTATION);
          break;
        case "Pattern":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "Pattern.java",
              "PatternValidator.java",
              TemplateIndexConstants.TEMPLATE_PATTERN_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_PATTERN_VALIDATOR_ANNOTATION);
          break;
        case "MultipleOf":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "MultipleOf.java",
              "MultipleOfValidator.java",
              TemplateIndexConstants.TEMPLATE_MULTIPLEOF_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_MULTIPLEOF_VALIDATOR_ANNOTATION);
          break;
        case "Maximum":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "Max.java",
              "MaxValidator.java",
              TemplateIndexConstants.TEMPLATE_MAX_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_MAX_VALIDATOR_ANNOTATION);
          break;
        case "Minimum":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "Min.java",
              "MinValidator.java",
              TemplateIndexConstants.TEMPLATE_MIN_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_MIN_VALIDATOR_ANNOTATION);
          break;
        case "MaxItems":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "MaxItems.java",
              "MaxItemsValidator.java",
              TemplateIndexConstants.TEMPLATE_MAX_ITEMS_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_MAX_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "MinItems":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "MinItems.java",
              "MinItemsValidator.java",
              TemplateIndexConstants.TEMPLATE_MIN_ITEMS_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_MIN_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "NotNull":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "NotNull.java",
              "NotNullValidator.java",
              TemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION);
          break;
        case "UniqueItems":
          templateFactory.fillTemplateCustom(
              filePathToSave,
              "UniqueItems.java",
              "UniqueItemsValidator.java",
              TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_ANNOTATION,
              TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_VALIDATOR_ANNOTATION);
          break;
        default:
          break;
      }
    }
  }

  private void checkRequiredOrCombinatorExists(final Map<String, SchemaObject> schemaList) {
    boolean shouldGenerateException = false;
    final var schemaListIt = schemaList.values().iterator();
    while (schemaListIt.hasNext() && !shouldGenerateException) {
      final var schema = schemaListIt.next();
      if ("anyOf".equals(schema.getSchemaCombinator())
          || "oneOf".equals(schema.getSchemaCombinator())) {
        shouldGenerateException = true;
      } else if (Objects.nonNull(schema.getFieldObjectList()) && !useLombok) {
        final var fieldListIt = schema.getFieldObjectList().iterator();
        while (fieldListIt.hasNext() && !shouldGenerateException) {
          final var field = fieldListIt.next();
          if (field.isRequired()) {
            shouldGenerateException = true;
          }
        }
      }
    }
    generateExceptionTemplate = shouldGenerateException;
  }
}
