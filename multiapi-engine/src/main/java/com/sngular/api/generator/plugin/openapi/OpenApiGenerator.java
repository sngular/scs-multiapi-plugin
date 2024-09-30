/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.PluginConstants;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.MapperContentUtil;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import com.sngular.api.generator.plugin.exception.GeneratorTemplateException;
import com.sngular.api.generator.plugin.openapi.exception.CodeGenerationException;
import com.sngular.api.generator.plugin.openapi.exception.DuplicateModelClassException;
import com.sngular.api.generator.plugin.openapi.model.AuthObject;
import com.sngular.api.generator.plugin.openapi.model.GlobalObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.openapi.template.TemplateFactory;
import com.sngular.api.generator.plugin.openapi.utils.MapperAuthUtil;
import com.sngular.api.generator.plugin.openapi.utils.MapperPathUtil;
import com.sngular.api.generator.plugin.openapi.utils.OpenApiUtil;
import freemarker.template.TemplateException;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.lang3.StringUtils;

public class OpenApiGenerator {

  private static final String SLASH = "/";

  private static final String DEFAULT_OPENAPI_API_PACKAGE = PluginConstants.DEFAULT_API_PACKAGE + ".openapi";

  private static final String DEFAULT_OPENAPI_MODEL_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".model";

  private static final String DEFAULT_OPENAPI_CLIENT_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".client";

  private final Boolean overwriteModel;

  private final Set<String> overwriteModelList = new HashSet<>();

  private final TemplateFactory templateFactory;

  private final String groupId;

  private final Path baseDir;

  private Boolean isWebClient = false;

  private Boolean isRestClient = false;

  private final List<String> authentications = new ArrayList<>();

  private final Integer springBootVersion;

  public OpenApiGenerator(
      final Integer springBootVersion,
      final Boolean overwriteModel,
      final File targetFolder,
      final String processedGeneratedSourcesFolder,
      final String groupId,
      final File basedir) {
    this.overwriteModel = overwriteModel;
    this.groupId = groupId;
    this.baseDir = basedir.toPath().toAbsolutePath();
    this.templateFactory = new TemplateFactory(overwriteModel, targetFolder, processedGeneratedSourcesFolder, basedir);
    this.springBootVersion = springBootVersion;
  }

  public final void processFileSpec(final List<SpecFile> specsListFile) {
    for (SpecFile specFile : specsListFile) {
      try {
        processPackage(specFile.getApiPackage());
        processFile(specFile);
        createClients(specFile);
        templateFactory.clearData();
      } catch (final IOException e) {
        throw new CodeGenerationException("Code generation failed. See above for the full exception.", e);
      }
    }
  }

  private void processFile(final SpecFile specFile) throws IOException {

    final JsonNode openAPI = OpenApiUtil.getPojoFromSpecFile(baseDir, specFile);
    final String clientPackage = specFile.getClientPackage();

    if (specFile.isCallMode()) {
      templateFactory.setWebClientPackageName(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE);
      templateFactory.setAuthPackageName((StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE) + ".auth");
      isWebClient = specFile.isReactive();
      isRestClient = !specFile.isReactive();
    }

    templateFactory.calculateJavaEEPackage(springBootVersion);
    final var globalObject = createApiTemplate(specFile, openAPI);

    createModelTemplate(specFile, openAPI, globalObject);
    templateFactory.fillTemplates();
  }

  private void createClients(final SpecFile specFile) {

    if (isWebClient || isRestClient) {
      try {
        final String clientPackage = specFile.getClientPackage();
        final String clientPath = StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE;
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

  private void createAuthTemplates(final SpecFile specFile) throws IOException {
    final String clientPackage = specFile.getClientPackage();
    final var authFileRoot = (StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE) + ".auth";

    templateFactory.setAuthPackageName(authFileRoot);
    templateFactory.fillTemplateAuth(authFileRoot, "Authentication");

    if (!authentications.isEmpty()) {
      for (String authentication : authentications) {
        templateFactory.fillTemplateAuth(authFileRoot, authentication);
      }
    }
  }

  private GlobalObject createApiTemplate(final SpecFile specFile, final JsonNode openAPI) {
    final MultiValuedMap<String, Map<String, JsonNode>> apis = OpenApiUtil.mapApiGroups(openAPI, specFile.isUseTagsGroup());
    final var authSchemaList = MapperAuthUtil.createAuthSchemaList(openAPI);
    final GlobalObject globalObject = MapperPathUtil.mapOpenApiObjectToOurModels(openAPI, authSchemaList);

    for (var apisKey : apis.keySet()) {
      final String javaFileName = OpenApiUtil.processJavaFileName(apisKey);
      final List<PathObject> pathObjects = MapperPathUtil.mapPathObjects(specFile, apis.get(apisKey), globalObject, baseDir);
      final AuthObject authObject = MapperAuthUtil.getApiAuthObject(globalObject.getAuthSchemas(), pathObjects);

      try {
        templateFactory.fillTemplate(specFile, javaFileName, pathObjects, authObject);
      } catch (IOException | TemplateException e) {
        throw new GeneratorTemplateException("Error filling the template", specFile.getFilePath(), e);
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

  private void createModelTemplate(final SpecFile specFile, final JsonNode openAPI, final GlobalObject globalObject) {
    final var modelPackage = processModelPackage(specFile.getModelPackage());

    final var totalSchemas = OpenApiUtil.processPaths(openAPI, globalObject.getSchemaMap());
    templateFactory.setModelPackageName(modelPackage);
    processModels(specFile, modelPackage, totalSchemas, overwriteModel);
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

  private void processModels(
      final SpecFile specFile, final String modelPackage, final Map<String, JsonNode> basicSchemaMap,
      final boolean overwrite) {
    basicSchemaMap.forEach((schemaName, basicSchema) -> {
      if (ApiTool.hasType(basicSchema)) {
        if (validType(ApiTool.getType(basicSchema))) {
          processModel(specFile, modelPackage, basicSchemaMap, overwrite, MapperUtil.getKeySchemaName(schemaName), basicSchema);
        }
      } else {
        processModel(specFile, modelPackage, basicSchemaMap, overwrite, MapperUtil.getKeySchemaName(schemaName), basicSchema);
      }
    });
  }

  private boolean validType(final String type) {
    return !TypeConstants.NO_PROCESS_TYPE.contains(type);
  }

  private void processModel(
      final SpecFile specFile, final String modelPackage, final Map<String, JsonNode> basicSchemaMap, final boolean overwrite,
      final String schemaName, final JsonNode basicSchema) {
    if (!overwrite && !overwriteModelList.add(schemaName + modelPackage)) {
      throw new DuplicateModelClassException(schemaName, modelPackage);
    }

    if (ApiTool.hasRef(basicSchema)) {
      final var refSchema = MapperUtil.getRefSchemaName(basicSchema);
      writeSchemaObject(specFile, refSchema, basicSchemaMap.get(refSchema), basicSchemaMap, modelPackage);
    } else if (!ApiTool.isArray(basicSchema) && !TypeConstants.STRING.equalsIgnoreCase(ApiTool.getType(basicSchema))) {
      writeSchemaObject(specFile, schemaName, basicSchema, basicSchemaMap, modelPackage);
    }
  }

  private void writeSchemaObject(
      final SpecFile specFile, final String schemaName, final JsonNode model, final Map<String, JsonNode> basicSchemaMap,
      final String modelPackage) {
    final String parentPackage = modelPackage.substring(modelPackage.lastIndexOf(".") + 1);
    final var schemaObjectIt = MapperContentUtil
        .mapComponentToSchemaObject(basicSchemaMap, schemaName, model, parentPackage, specFile, this.baseDir).iterator();
    if (schemaObjectIt.hasNext()) {
      writeSchemaObject(specFile.isUseLombokModelAnnotation(), specFile.getModelPackage(), schemaName, schemaObjectIt.next());
    }
    schemaObjectIt.forEachRemaining(schemaObj -> writeSchemaObject(specFile.isUseLombokModelAnnotation(), specFile.getModelPackage(), null, schemaObj));

  }

  private void writeSchemaObject(final boolean usingLombok, final String modelPackageReceived, final String keyClassName, final SchemaObject schemaObject) {
    final var finalModelPackageReceived = StringUtils.defaultIfEmpty(modelPackageReceived, DEFAULT_OPENAPI_API_PACKAGE);
    final var destinationPackage = StringUtils.defaultIfEmpty(finalModelPackageReceived, DEFAULT_OPENAPI_API_PACKAGE + SLASH + schemaObject.getParentPackage());
    templateFactory.addSchemaObject(finalModelPackageReceived, keyClassName, schemaObject, destinationPackage, usingLombok);
    templateFactory.checkRequiredOrCombinatorExists(schemaObject, usingLombok);
  }
}
