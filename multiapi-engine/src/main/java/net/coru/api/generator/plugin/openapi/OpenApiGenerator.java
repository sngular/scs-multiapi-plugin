/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

import freemarker.template.TemplateException;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;
import net.coru.api.generator.plugin.PluginConstants;
import net.coru.api.generator.plugin.openapi.exception.CodeGenerationException;
import net.coru.api.generator.plugin.openapi.exception.DuplicateModelClassException;
import net.coru.api.generator.plugin.openapi.model.AuthObject;
import net.coru.api.generator.plugin.openapi.model.GlobalObject;
import net.coru.api.generator.plugin.openapi.model.PathObject;
import net.coru.api.generator.plugin.openapi.model.SchemaObject;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import net.coru.api.generator.plugin.openapi.template.TemplateFactory;
import net.coru.api.generator.plugin.openapi.utils.MapperAuthUtil;
import net.coru.api.generator.plugin.openapi.utils.MapperContentUtil;
import net.coru.api.generator.plugin.openapi.utils.MapperPathUtil;
import net.coru.api.generator.plugin.openapi.utils.OpenApiUtil;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

public class OpenApiGenerator {

  private static final String DEFAULT_OPENAPI_API_PACKAGE = PluginConstants.DEFAULT_API_PACKAGE + ".openapi";

  private static final String DEFAULT_OPENAPI_MODEL_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".model";

  private static final String DEFAULT_OPENAPI_CLIENT_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".client";

  public static final Pattern PACKAGE_SEPARATOR = Pattern.compile("\\.");

  private final Boolean overwriteModel;

  private boolean generateExceptionTemplate;

  private final FilenameFilter targetFileFilter;

  private final Set<String> overwriteModelList = new HashSet<>();

  private final TemplateFactory templateFactory;

  private final String processedGeneratedSourcesFolder;

  private final String groupId;

  private final File targetFolder;

  private final File baseDir;

  private Boolean isWebClient = false;

  private Boolean isRestClient = false;

  private final List<String> authentications = new ArrayList<>();

  private boolean useLombok;

  public OpenApiGenerator(final Boolean overwriteModel, final String processedGeneratedSourcesFolder, final String groupId,
      final File targetFolder, final File basedir) {
    templateFactory = new TemplateFactory();
    this.overwriteModel = overwriteModel;
    this.processedGeneratedSourcesFolder = processedGeneratedSourcesFolder;
    this.groupId = groupId;
    this.targetFolder = targetFolder;
    this.baseDir = basedir;
    this.targetFileFilter = (dir, name) -> name.toLowerCase().contains(targetFolder.toPath().getFileName().toString());
  }

  public void processFileSpec(final List<FileSpec> fileSpecsList) {

    for (FileSpec fileSpec : fileSpecsList) {
      generateExceptionTemplate = false;
      useLombok = Boolean.TRUE.equals(fileSpec.isUseLombokModelAnnotation());
      try {
        processPackage(fileSpec.getApiPackage());
        final String filePathToSave = processPath(fileSpec.getApiPackage(), false);
        processFile(fileSpec, filePathToSave);
        createClients(fileSpec);
      } catch (final TemplateException | IOException e) {
        throw new CodeGenerationException("Code generation failed. See above for the full exception.", e);
      }
    }
  }

  private void processFile(final FileSpec fileSpec, final String filePathToSave) throws TemplateException, IOException {

    final OpenAPI openAPI = OpenApiUtil.getPojoFromSwagger(fileSpec);
    final String clientPackage = fileSpec.getClientPackage();

    if (fileSpec.isCallMode()) {
      templateFactory.setWebClientPackageName(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE);
      templateFactory.setAuthPackageName((StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE) + ".auth");
      isWebClient = fileSpec.isReactive();
      isRestClient = !fileSpec.isReactive();
    }

    createApiTemplate(fileSpec, filePathToSave, openAPI);

    createModelTemplate(fileSpec, openAPI);

  }

  private void createClients(final FileSpec fileSpec) {
    final String clientPackage = fileSpec.getClientPackage();

    if (isWebClient || isRestClient) {
      final String clientPath = processPath(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE, false);
      try {
        if (Boolean.TRUE.equals(isWebClient)) {
          templateFactory.fillTemplateWebClient(clientPath);
        }
        if (Boolean.TRUE.equals(isRestClient)) {
          templateFactory.fillTemplateRestClient(clientPath);
        }
        createAuthTemplates(fileSpec);
      } catch (IOException | TemplateException e) {
        e.printStackTrace();
      }
    }
  }

  private void createAuthTemplates(final FileSpec fileSpec) throws TemplateException, IOException {
    final String clientPackage = fileSpec.getClientPackage();
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

  private void createApiTemplate(final FileSpec fileSpec, final String filePathToSave, final OpenAPI openAPI) {
    final Map<String, HashMap<String, PathItem>> apis = OpenApiUtil.mapApiGroups(openAPI, fileSpec.isUseTagsGroup());
    final var authSchemaList = MapperAuthUtil.createAuthSchemaList(openAPI);
    final GlobalObject globalObject = MapperPathUtil.mapOpenApiObjectToOurModels(openAPI, fileSpec, authSchemaList);

    for (Map.Entry<String, HashMap<String, PathItem>> apisEntry : apis.entrySet()) {
      final String javaFileName = OpenApiUtil.processJavaFileName(apisEntry.getKey());
      final List<PathObject> pathObjects = MapperPathUtil.mapPathObjects(openAPI, fileSpec, apisEntry, globalObject);
      final AuthObject authObject = MapperAuthUtil.getApiAuthObject(globalObject.getAuthSchemas(), pathObjects);

      try {
        templateFactory.fillTemplate(filePathToSave, fileSpec, javaFileName, pathObjects, authObject);
      } catch (IOException | TemplateException e) {
        e.printStackTrace();
      }

      if (Boolean.TRUE.equals(fileSpec.isCallMode())) {
        addAuthentications(authObject);
      }
    }
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

  private void createModelTemplate(final FileSpec fileSpec, final OpenAPI openAPI) throws TemplateException, IOException {

    final String fileModelToSave = processPath(fileSpec.getModelPackage(), true);
    final var modelPackage = processModelPackage(fileSpec.getModelPackage());
    final var basicSchemaMap = OpenApiUtil.processBasicSchemas(openAPI);
    templateFactory.setModelPackageName(modelPackage);

    if (Boolean.TRUE.equals(overwriteModel)) {
      processModelsWhenOverWriteIsTrue(fileSpec, openAPI, fileModelToSave, modelPackage, basicSchemaMap);
    } else {
      processWhenOverwriteModelIsFalse(fileSpec, openAPI, fileModelToSave, modelPackage, basicSchemaMap);
    }
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

  private String processPath(final String fileSpecPackage, final Boolean isModel) {
    Path path;
    final File[] pathList = Objects.requireNonNull(baseDir.listFiles(targetFileFilter));
    if (pathList.length > 0) {
      path = pathList[0].toPath().resolve(convertPackageToTargetPath(fileSpecPackage, isModel));
    } else {
      path = targetFolder.toPath();
      path.toFile().mkdirs();
      path = path.resolve(convertPackageToTargetPath(fileSpecPackage, isModel));
    }
    if (!path.toFile().isDirectory()) {
      path.toFile().mkdirs();
    }
    return path.toString();
  }

  private String convertPackageToTargetPath(final String fileSpecPackage, final Boolean isModel) {
    final String path;
    if (StringUtils.isNotBlank(fileSpecPackage)) {
      path = FilenameUtils.concat(processedGeneratedSourcesFolder, PACKAGE_SEPARATOR.matcher(fileSpecPackage.trim()).replaceAll("/"));
    } else if (groupId != null) {
      path = FilenameUtils.concat(processedGeneratedSourcesFolder, PACKAGE_SEPARATOR.matcher(groupId).replaceAll(File.separator));
    } else {
      final String pathDefault = Boolean.TRUE.equals(isModel) ? DEFAULT_OPENAPI_MODEL_PACKAGE : DEFAULT_OPENAPI_API_PACKAGE;
      path = FilenameUtils.concat(processedGeneratedSourcesFolder, PACKAGE_SEPARATOR.matcher(pathDefault).replaceAll(File.separator));
    }
    return path;
  }

  private void processModelsWhenOverWriteIsTrue(
      final FileSpec fileSpec, final OpenAPI openAPI, final String fileModelToSave,
      final String modelPackage,
      final Map<String, Schema<?>> basicSchemaMap) throws TemplateException, IOException {

    basicSchemaMap.forEach((schemaName, basicSchema) -> {
      final var basicSchemaObject = MapperContentUtil.mapComponentToSchemaObject(openAPI.getComponents().getSchemas(), basicSchema, schemaName, fileSpec, modelPackage);
      checkRequiredOrCombinatorExists(basicSchemaObject);
      try {
        templateFactory.fillTemplateSchema(fileModelToSave, fileSpec.isUseLombokModelAnnotation(), basicSchemaObject);
      } catch (IOException | TemplateException e) {
        e.printStackTrace();
      }
    });

    if (Boolean.TRUE.equals(generateExceptionTemplate)) {
      templateFactory.fillTemplateModelClassException(fileModelToSave);
    }
  }

  private void checkRequiredOrCombinatorExists(final SchemaObject schema) {
    if ("anyOf".equals(schema.getSchemaCombinator()) || "oneOf".equals(schema.getSchemaCombinator())) {
      generateExceptionTemplate = true;
    } else if (Objects.nonNull(schema.getFieldObjectList()) && !useLombok) {
      final var fieldListIt = schema.getFieldObjectList().listIterator();
      if (fieldListIt.hasNext()) {
        do {
          final var field = fieldListIt.next();
          if (field.isRequired()) {
            generateExceptionTemplate = true;
          }
        } while (fieldListIt.hasNext() && !generateExceptionTemplate);
      }
    }
  }

  private void processWhenOverwriteModelIsFalse(
      final FileSpec fileSpec, final OpenAPI openAPI,
      final String fileModelToSave, final String modelPackage, final Map<String, Schema<?>> basicSchemaMap) throws TemplateException, IOException {

    for (Entry<String, Schema<?>> entry : basicSchemaMap.entrySet()) {
      final String schemaName = entry.getKey();
      final Schema<?> basicSchema = entry.getValue();
      if (!overwriteModelList.add(schemaName + modelPackage)) {
        throw new DuplicateModelClassException(schemaName, modelPackage);
      }
      final var basicSchemaObject = MapperContentUtil.mapComponentToSchemaObject(openAPI.getComponents().getSchemas(), basicSchema, schemaName,
                                                                                 fileSpec, modelPackage);
      checkRequiredOrCombinatorExists(basicSchemaObject);
      try {
        templateFactory.fillTemplateSchema(fileModelToSave, fileSpec.isUseLombokModelAnnotation(), basicSchemaObject);
      } catch (IOException | TemplateException e) {
        e.printStackTrace();
      }
    }
    if (Boolean.TRUE.equals(generateExceptionTemplate)) {
      templateFactory.fillTemplateModelClassException(fileModelToSave);
    }
  }
}
