/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin;

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

import freemarker.template.TemplateException;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;
import lombok.extern.slf4j.Slf4j;
import net.coru.api.generator.plugin.openapi.exception.DuplicateModelClassException;
import net.coru.api.generator.plugin.openapi.exception.OpenApiGeneratedSourceFolderException;
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
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

@Mojo(name = "openapi-generation", defaultPhase = LifecyclePhase.GENERATE_SOURCES, requiresDependencyResolution = ResolutionScope.COMPILE)
@Slf4j
public final class OpenapiMultiFileMojo extends AbstractMojo {

  private static final String DEFAULT_OPENAPI_API_PACKAGE = PluginConstants.DEFAULT_API_PACKAGE + ".openapi";

  private static final String DEFAULT_OPENAPI_MODEL_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".model";

  private static final String DEFAULT_OPENAPI_CLIENT_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".client";

  private Boolean generateExceptionTemplate = false;

  @Parameter(defaultValue = "${project}", required = true, readonly = true)
  private MavenProject project;

  @Parameter(defaultValue = "${project.build.directory}", required = true, readonly = true)
  private File targetFolder;

  @Parameter(property = "fileSpecs")
  private List<FileSpec> fileSpecs;

  @Parameter(name = "overwriteModel", property = "overwriteModel", defaultValue = "true")
  private Boolean overwriteModel;

  @Parameter(name = "generatedSourcesFolder", property = "generatedSourcesFolder", defaultValue = PluginConstants.GENERATED_SOURCES_FOLDER)
  private String generatedSourcesFolder;

  private final FilenameFilter targetFileFilter = (dir, name) -> name.toLowerCase().contains(targetFolder.toPath().getFileName().toString());

  private final Set<String> overwriteModelList = new HashSet<>();

  private TemplateFactory templateFactory;

  private Boolean isWebClient = false;

  private Boolean isRestClient = false;

  private final List<String> authentications = new ArrayList<>();

  private String processedGeneratedSourcesFolder;

  @Override
  public void execute() throws MojoExecutionException {
    processGeneratedSourcesFolderName();
    addGeneratedSourcesToProject();

    templateFactory = new TemplateFactory();
    if (null != fileSpecs && !fileSpecs.isEmpty()) {
      processFileSpec(fileSpecs);
    } else {
      throw new MojoExecutionException("Code generation failed. Not exists FileSpec configuration to generate package and class");
    }

  }

  private void processGeneratedSourcesFolderName() {
    if (generatedSourcesFolder.matches("[a-zA-Z\\d\\-]+")) {
      processedGeneratedSourcesFolder = generatedSourcesFolder + "/" + PluginConstants.GENERATED_SOURCES_API_GENERATOR_FOLDER;
    } else {
      throw new OpenApiGeneratedSourceFolderException(generatedSourcesFolder);
    }
  }

  private void addGeneratedSourcesToProject() {
    final Path projectPath = targetFolder.toPath().resolve(processedGeneratedSourcesFolder);
    project.addCompileSourceRoot(projectPath.toString());
  }

  private void processFileSpec(final List<FileSpec> fileSpecsList) throws MojoExecutionException {

    for (FileSpec fileSpec : fileSpecsList) {
      try {
        processPackage(fileSpec.getApiPackage());
        final String filePathToSave = processPath(fileSpec.getApiPackage(), false);
        processFile(fileSpec, filePathToSave);
        createClients(fileSpec);
      } catch (final MojoExecutionException | TemplateException | IOException e) {
        getLog().error(e);
        throw new MojoExecutionException("Code generation failed. See above for the full exception.");
      }
    }
  }

  private void processFile(final FileSpec fileSpec, final String filePathToSave) throws MojoExecutionException, TemplateException, IOException {

    final OpenAPI openAPI = OpenApiUtil.getPojoFromSwagger(fileSpec);
    final String clientPackage = fileSpec.getClientPackage();

    if (Boolean.TRUE.equals(fileSpec.getCallMode())) {
      templateFactory.setWebClientPackageName(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE);
      templateFactory.setAuthPackageName((StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE) + ".auth");
      isWebClient = fileSpec.getIsReactive();
      isRestClient = !fileSpec.getIsReactive();
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

    if (null != authentications && !authentications.isEmpty()) {
      for (String authentication : authentications) {
        templateFactory.fillTemplateAuth(authFileToSave, authentication);
      }
    }
  }

  private void createApiTemplate(final FileSpec fileSpec, final String filePathToSave, final OpenAPI openAPI) {
    final Map<String, HashMap<String, PathItem>> apis = OpenApiUtil.mapApiGroups(openAPI, fileSpec.getUseTagsGroup());
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

      if (Boolean.TRUE.equals(fileSpec.getCallMode())) {
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
    } else if (project.getModel().getGroupId() != null) {
      templateFactory.setPackageName(project.getModel().getGroupId());
    } else {
      templateFactory.setPackageName(DEFAULT_OPENAPI_API_PACKAGE);
    }
  }

  private String processModelPackage(final String modelPackage) {
    var modelReturnPackage = "";
    if (StringUtils.isNotBlank(modelPackage)) {
      modelReturnPackage = modelPackage.trim();
    } else if (project.getModel().getGroupId() != null) {
      modelReturnPackage = project.getModel().getGroupId() + ".model";
    } else {
      modelReturnPackage = DEFAULT_OPENAPI_MODEL_PACKAGE;
    }
    return modelReturnPackage;
  }

  private String processPath(final String fileSpecPackage, final Boolean isModel) {
    Path path;
    final File[] pathList = Objects.requireNonNull(project.getBasedir().listFiles(targetFileFilter));
    if (pathList.length > 0) {
      path = pathList[0].toPath().resolve(convertPackageToTargetPath(fileSpecPackage, isModel));
    } else {
      path = targetFolder.toPath();
      path.toFile().mkdir();
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
      path = processedGeneratedSourcesFolder + fileSpecPackage.trim().replaceAll("\\.", "/");
    } else if (project.getModel().getGroupId() != null) {
      path = processedGeneratedSourcesFolder + project.getModel().getGroupId().replaceAll("\\.", "/");
    } else {
      final String pathDefault = Boolean.TRUE.equals(isModel) ? DEFAULT_OPENAPI_MODEL_PACKAGE : DEFAULT_OPENAPI_API_PACKAGE;
      path = processedGeneratedSourcesFolder + pathDefault.replaceAll("\\.", "/");
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
        templateFactory.fillTemplateSchema(fileModelToSave, fileSpec.getUseLombokModelAnnotation(), basicSchemaObject);
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
    } else if (Objects.nonNull(schema.getFieldObjectList())) {
      for (var field : schema.getFieldObjectList()) {
        if (field.getRequired()) {
          generateExceptionTemplate = true;
        }
      }
    }
  }

  private void processWhenOverwriteModelIsFalse(
      final FileSpec fileSpec, final OpenAPI openAPI,
      final String fileModelToSave, final String modelPackage, final Map<String, Schema<?>> basicSchemaMap) {

    for (Entry<String, Schema<?>> entry : basicSchemaMap.entrySet()) {
      final String schemaName = entry.getKey();
      final Schema<?> basicSchema = entry.getValue();
      if (!overwriteModelList.add(schemaName + modelPackage)) {
        throw new DuplicateModelClassException(schemaName, modelPackage);
      }
      try {
        templateFactory.fillTemplateSchema(fileModelToSave, fileSpec.getUseLombokModelAnnotation(),
                                           MapperContentUtil.mapComponentToSchemaObject(openAPI.getComponents().getSchemas(), basicSchema, schemaName,
                                                                                        fileSpec, modelPackage));
      } catch (IOException | TemplateException e) {
        e.printStackTrace();
      }
    }
  }

}
