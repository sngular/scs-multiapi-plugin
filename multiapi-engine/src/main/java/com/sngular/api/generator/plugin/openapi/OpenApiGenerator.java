/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.PluginConstants;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.exception.GeneratedSourcesException;
import com.sngular.api.generator.plugin.exception.GeneratorTemplateException;
import com.sngular.api.generator.plugin.openapi.exception.CodeGenerationException;
import com.sngular.api.generator.plugin.openapi.exception.DuplicateModelClassException;
import com.sngular.api.generator.plugin.openapi.model.AuthObject;
import com.sngular.api.generator.plugin.openapi.model.GlobalObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import com.sngular.api.generator.plugin.openapi.model.SchemaObject;
import com.sngular.api.generator.plugin.openapi.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.openapi.template.TemplateFactory;
import com.sngular.api.generator.plugin.openapi.template.TemplateIndexConstants;
import com.sngular.api.generator.plugin.openapi.utils.MapperAuthUtil;
import com.sngular.api.generator.plugin.openapi.utils.MapperContentUtil;
import com.sngular.api.generator.plugin.openapi.utils.MapperPathUtil;
import com.sngular.api.generator.plugin.openapi.utils.MapperUtil;
import com.sngular.api.generator.plugin.openapi.utils.OpenApiUtil;
import freemarker.template.TemplateException;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

public class OpenApiGenerator {

  private static final String DEFAULT_OPENAPI_API_PACKAGE = PluginConstants.DEFAULT_API_PACKAGE + ".openapi";

  private static final String DEFAULT_OPENAPI_MODEL_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".model";

  private static final String DEFAULT_OPENAPI_CLIENT_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".client";

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
      final Integer springBootVersion, final Boolean overwriteModel, final String processedGeneratedSourcesFolder, final String groupId,
      final File targetFolder, final File basedir) {
    templateFactory = new TemplateFactory();
    this.overwriteModel = overwriteModel;
    this.processedGeneratedSourcesFolder = processedGeneratedSourcesFolder;
    this.groupId = groupId;
    this.targetFolder = targetFolder;
    this.baseDir = basedir.toPath().toAbsolutePath();
    this.targetFileFilter = (dir, name) -> name.toLowerCase().contains(targetFolder.toPath().getFileName().toString());
    this.springBootVersion = springBootVersion;
  }

  public final void processFileSpec(final List<SpecFile> specsListFile) {
    for (SpecFile specFile : specsListFile) {
      generateExceptionTemplate = false;
      useLombok = Boolean.TRUE.equals(specFile.isUseLombokModelAnnotation());
      try {
        processPackage(specFile.getApiPackage());
        final String filePathToSave = processPath(specFile.getApiPackage(), false);
        processFile(specFile, filePathToSave);
        createClients(specFile);
      } catch (final IOException e) {
        throw new CodeGenerationException("Code generation failed. See above for the full exception.", e);
      }
    }
  }

  private void processFile(final SpecFile specFile, final String filePathToSave) throws IOException {

    final JsonNode openAPI = OpenApiUtil.getPojoFromSpecFile(baseDir, specFile);
    final String clientPackage = specFile.getClientPackage();

    if (specFile.isCallMode()) {
      templateFactory.setWebClientPackageName(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE);
      templateFactory.setAuthPackageName((StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE) + ".auth");
      isWebClient = specFile.isReactive();
      isRestClient = !specFile.isReactive();
    }

    templateFactory.calculateJavaEEPackage(springBootVersion);
    final var globalObject = createApiTemplate(specFile, filePathToSave, openAPI);

    createModelTemplate(specFile, openAPI, globalObject);

  }

  private void createClients(final SpecFile specFile) {

    if (isWebClient || isRestClient) {
      try {
        final String clientPackage = specFile.getClientPackage();
        final String clientPath = processPath(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE, false);
        if (Boolean.TRUE.equals(isWebClient)) {
          templateFactory.fillTemplateWebClient(clientPath);
        }
        if (Boolean.TRUE.equals(isRestClient)) {
          templateFactory.fillTemplateRestClient(clientPath);
        }
        createAuthTemplates(specFile);
      } catch (IOException | TemplateException e) {
        throw new GeneratorTemplateException("Template Generator problem", e);
      }
    }
  }

  private void createAuthTemplates(final SpecFile specFile) throws TemplateException, IOException {
    final String clientPackage = specFile.getClientPackage();
    final var authFileRoot = (StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE) + ".auth";
    final String authFileToSave = processPath(authFileRoot, false);

    templateFactory.setAuthPackageName(authFileRoot);
    templateFactory.fillTemplateAuth(authFileToSave, "Authentication");

    if (!authentications.isEmpty()) {
      for (String authentication : authentications) {
        templateFactory.fillTemplateAuth(authFileToSave, authentication);
      }
    }
  }

  private GlobalObject createApiTemplate(final SpecFile specFile, final String filePathToSave, final JsonNode openAPI) {
    final MultiValuedMap<String, Map<String, JsonNode>> apis = OpenApiUtil.mapApiGroups(openAPI, specFile.isUseTagsGroup());
    final var authSchemaList = MapperAuthUtil.createAuthSchemaList(openAPI);
    final GlobalObject globalObject = MapperPathUtil.mapOpenApiObjectToOurModels(openAPI, authSchemaList);

    for (var apisKey : apis.keySet()) {
      final String javaFileName = OpenApiUtil.processJavaFileName(apisKey);
      final List<PathObject> pathObjects = MapperPathUtil.mapPathObjects(specFile, apis.get(apisKey), globalObject, baseDir);
      final AuthObject authObject = MapperAuthUtil.getApiAuthObject(globalObject.getAuthSchemas(), pathObjects);

      try {
        templateFactory.fillTemplate(filePathToSave, specFile, javaFileName, pathObjects, authObject);
      } catch (IOException | TemplateException e) {
        throw new GeneratorTemplateException("Error filling the template", e);
      }

      if (Boolean.TRUE.equals(specFile.isCallMode())) {
        addAuthentications(authObject);
      }
    }

    return globalObject;
  }

  private void addAuthentications(final AuthObject authObject) {

    if (null != authObject.getSecurityRequirements() && !authObject.getSecurityRequirements().isEmpty()) {
      authObject.getSecurityRequirements().forEach(authType -> {
        if (!authentications.contains(authType)) {
          authentications.add(authType);
        }
      });
    }
  }

  private void createModelTemplate(final SpecFile specFile, final JsonNode openAPI, final GlobalObject globalObject) throws IOException {
    final String fileModelToSave = processPath(specFile.getModelPackage(), true);
    final var modelPackage = processModelPackage(specFile.getModelPackage());
    final var basicSchemaMap = OpenApiUtil.processBasicJsonNodes(openAPI, globalObject.getSchemaMap());
    templateFactory.setModelPackageName(modelPackage);
    processModels(specFile, fileModelToSave, modelPackage, basicSchemaMap, Boolean.TRUE.equals(overwriteModel));
  }

  private void processPackage(final String apiPackage) {
    if (StringUtils.isNotBlank(apiPackage)) {
      templateFactory.setPackageName(apiPackage.trim());
    } else {
      templateFactory.setPackageName(Objects.requireNonNullElse(groupId, DEFAULT_OPENAPI_API_PACKAGE));
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

  private String processPath(final String fileSpecPackage, final boolean isModel) throws IOException {
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
    final String toMatch = StringUtils.defaultIfBlank(fileSpecPackage, StringUtils.defaultIfBlank(groupId, isModel ? DEFAULT_OPENAPI_MODEL_PACKAGE : DEFAULT_OPENAPI_API_PACKAGE));
    return FilenameUtils.concat(processedGeneratedSourcesFolder, PACKAGE_SEPARATOR.matcher(toMatch).replaceAll("/"));
  }

  @SuppressWarnings("checkstyle:LambdaBodyLength")
  private void processModels(
      final SpecFile specFile, final String fileModelToSave, final String modelPackage, final Map<String, JsonNode> basicSchemaMap,
      final boolean overwrite) {
    final Map<String, SchemaObject> builtSchemasMap = new HashMap<>();
    basicSchemaMap.forEach((schemaName, basicSchema) -> {
      if (!overwrite && !overwriteModelList.add(schemaName + modelPackage)) {
        throw new DuplicateModelClassException(schemaName, modelPackage);
      }

      if (ApiTool.hasRef(basicSchema)) {
        final var refSchema = MapperUtil.getRefSchemaName(basicSchema);
        builtSchemasMap.putAll(writeModel(specFile, fileModelToSave, refSchema, basicSchemaMap.get(refSchema), basicSchemaMap, builtSchemasMap));
      } else if (!ApiTool.isArray(basicSchema) && !TypeConstants.STRING.equalsIgnoreCase(ApiTool.getType(basicSchema))) {
        builtSchemasMap.putAll(writeModel(specFile, fileModelToSave, schemaName, basicSchema, basicSchemaMap, builtSchemasMap));
      }
    });

  }

  private Map<String, SchemaObject> writeModel(
      final SpecFile specFile, final String fileModelToSave, final String schemaName, final JsonNode basicSchema, final Map<String, JsonNode> basicSchemaMap,
      final Map<String, SchemaObject> builtSchemasMap) {
    final var schemaObjectMap = MapperContentUtil
        .mapComponentToSchemaObject(basicSchemaMap, builtSchemasMap, basicSchema, schemaName, specFile, baseDir);
    checkRequiredOrCombinatorExists(schemaObjectMap);
    schemaObjectMap.values().forEach(schemaObject -> {
      try {
        final Set<String> propertiesSet = new HashSet<>();
        templateFactory.fillTemplateSchema(fileModelToSave, specFile.isUseLombokModelAnnotation(), schemaObject, propertiesSet);
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
  private void fillTemplates(final String filePathToSave, final Set<String> fieldProperties) throws TemplateException, IOException {
    for (final String current : fieldProperties) {
      switch (current) {
        case "Size":
          templateFactory.fillTemplateCustom(filePathToSave, "Size.java", "SizeValidator.java", TemplateIndexConstants.TEMPLATE_SIZE_ANNOTATION,
                                             TemplateIndexConstants.TEMPLATE_SIZE_VALIDATOR_ANNOTATION);
          break;
        case "Pattern":
          templateFactory.fillTemplateCustom(filePathToSave, "Pattern.java", "PatternValidator.java", TemplateIndexConstants.TEMPLATE_PATTERN_ANNOTATION,
                                             TemplateIndexConstants.TEMPLATE_PATTERN_VALIDATOR_ANNOTATION);
          break;
        case "MultipleOf":
          templateFactory.fillTemplateCustom(filePathToSave, "MultipleOf.java", "MultipleOfValidator.java", TemplateIndexConstants.TEMPLATE_MULTIPLEOF_ANNOTATION,
                                             TemplateIndexConstants.TEMPLATE_MULTIPLEOF_VALIDATOR_ANNOTATION);
          break;
        case "Maximum":
          templateFactory.fillTemplateCustom(filePathToSave, "Max.java", "MaxValidator.java", TemplateIndexConstants.TEMPLATE_MAX_ANNOTATION,
                                             TemplateIndexConstants.TEMPLATE_MAX_VALIDATOR_ANNOTATION);
          break;
        case "Minimum":
          templateFactory.fillTemplateCustom(filePathToSave, "Min.java", "MinValidator.java", TemplateIndexConstants.TEMPLATE_MIN_ANNOTATION,
                                             TemplateIndexConstants.TEMPLATE_MIN_VALIDATOR_ANNOTATION);
          break;
        case "MaxItems":
          templateFactory.fillTemplateCustom(filePathToSave, "MaxItems.java", "MaxItemsValidator.java", TemplateIndexConstants.TEMPLATE_MAX_ITEMS_ANNOTATION,
                                             TemplateIndexConstants.TEMPLATE_MAX_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "MinItems":
          templateFactory.fillTemplateCustom(filePathToSave, "MinItems.java", "MinItemsValidator.java", TemplateIndexConstants.TEMPLATE_MIN_ITEMS_ANNOTATION,
                                             TemplateIndexConstants.TEMPLATE_MIN_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "NotNull":
          templateFactory.fillTemplateCustom(filePathToSave, "NotNull.java", "NotNullValidator.java", TemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION,
                                             TemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION);
          break;
        case "UniqueItems":
          templateFactory.fillTemplateCustom(filePathToSave, "UniqueItems.java", "UniqueItemsValidator.java", TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_ANNOTATION,
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
      if ("anyOf".equals(schema.getSchemaCombinator()) || "oneOf".equals(schema.getSchemaCombinator())) {
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
