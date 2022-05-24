package com.corunet.api.generator.plugin;

import static com.corunet.api.generator.plugin.PluginConstraints.DEFAULT_TARGET_PACKAGE;
import static com.corunet.api.generator.plugin.PluginConstraints.GENERATED_SOURCES_PATH;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.corunet.api.generator.plugin.openapi.model.AuthObject;
import com.corunet.api.generator.plugin.openapi.model.GlobalObject;
import com.corunet.api.generator.plugin.openapi.model.PathObject;
import com.corunet.api.generator.plugin.openapi.FileSpec;
import com.corunet.api.generator.plugin.openapi.OpenApiUtil;
import com.corunet.api.generator.plugin.openapi.TemplateFactory;
import freemarker.template.TemplateException;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Mojo(name = "openapi-generation", defaultPhase = LifecyclePhase.GENERATE_SOURCES, requiresDependencyResolution = ResolutionScope.COMPILE)
public class OpenapiMultiFileMojo extends AbstractMojo {

  private static final String DEFAULT_OPENAPI_TARGET_PACKAGE = DEFAULT_TARGET_PACKAGE + ".openapi";

  private static final String DEFAULT_OPENAPI_MODEL_PACKAGE = DEFAULT_OPENAPI_TARGET_PACKAGE + ".model";

  private static final String DEFAULT_OPENAPI_CLIENT_PACKAGE = DEFAULT_OPENAPI_TARGET_PACKAGE + ".client";

  private final FilenameFilter targetFileFilter = (dir, name) -> name.toLowerCase().contains("target");

  private final Logger LOGGER = LoggerFactory.getLogger(OpenapiMultiFileMojo.class);

  @Parameter(defaultValue = "${project}", required = true, readonly = true)
  public MavenProject project;

  @Parameter(property = "fileSpecs")
  public List<FileSpec> fileSpecs;

  @Parameter(name = "clientPackage", property = "clientPackage")
  private String clientPackage;

  private TemplateFactory templateFactory;

  private Boolean isWebClient = false;

  private Boolean isRestClient = false;

  private List<String> authentications = new ArrayList<>();

  @Override
  public void execute() throws MojoExecutionException {
    templateFactory = new TemplateFactory();
    if (null != fileSpecs && !fileSpecs.isEmpty()) {
      processFileSpec(fileSpecs);
      createClients();
    } else {
      throw new MojoExecutionException("Code generation failed. Not exists FileSpec configuration to generate package and class");
    }

  }

  private void processFileSpec(List<FileSpec> fileSpecsList) throws MojoExecutionException {

    for (FileSpec fileSpec : fileSpecsList) {
      try {
        processPackage(fileSpec.getApiPackage());
        String filePathToSave = processPath(fileSpec.getApiPackage(), false);
        project.addCompileSourceRoot(filePathToSave);
        processFile(fileSpec, filePathToSave);

      } catch (Exception e) {
        getLog().error(e);
        throw new MojoExecutionException("Code generation failed. See above for the full exception.");
      }
    }
  }

  private void processFile(FileSpec fileSpec, String filePathToSave) throws MojoExecutionException {

    try {

      OpenAPI openAPI = OpenApiUtil.getPojoFromSwagger(fileSpec);

      if (fileSpec.getCallMode()) {
        templateFactory.setWebClientPackageName(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE);
        templateFactory.setAuthPackageName((StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE) + ".auth");
        isWebClient = fileSpec.getIsReactive();
        isRestClient = fileSpec.getIsReactive() ? false : true;
      }

      createApiTemplate(fileSpec, filePathToSave, openAPI);

      createModelTemplate(fileSpec, openAPI);

    } catch (Exception e) {
      getLog().error(e);
      throw new MojoExecutionException("Code generation failed: " + e.getMessage());
    }

  }

  private void createClients() {

    if (isWebClient || isRestClient) {
      String clientPath = processPath(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE, false);
      project.addCompileSourceRoot(clientPath);
      try {
        if (isWebClient) {
          templateFactory.fillTemplateWebClient(clientPath);
        }

        if (isRestClient) {
          templateFactory.fillTemplateRestClient(clientPath);
        }

        createAuthTemplates();
      } catch (Exception e) {
        e.printStackTrace();
      }

    }
  }

  private void createAuthTemplates() throws TemplateException, IOException {

    var authFileRoot = StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE + ".auth";
    String authFileToSave = processPath(authFileRoot, false);
    templateFactory.setAuthPackageName(authFileRoot);
    templateFactory.fillTemplateAuth(authFileToSave, "Authentication");

    if (null != authentications && !authentications.isEmpty()) {
      authentications.forEach(authentication -> {
        try {
          templateFactory.fillTemplateAuth(authFileToSave, authentication);
        } catch (IOException | TemplateException e) {
          throw new RuntimeException(e);
        }
      });
    }
  }

  private void createApiTemplate(FileSpec fileSpec, String filePathToSave, OpenAPI openAPI) throws Exception {
    HashMap<String, HashMap<String, PathItem>> apis = OpenApiUtil.mapApiGroups(openAPI, fileSpec.getUseTagsGroup());
    templateFactory.addComponents(openAPI.getComponents().getSchemas());
    GlobalObject globalObject = OpenApiUtil.mapOpenApiObjectToOurModels(openAPI, fileSpec);

    for (Map.Entry<String, HashMap<String, PathItem>> apisEntry : apis.entrySet()) {
      templateFactory.addPathItems(apisEntry.getValue());
      ArrayList<PathObject> pathObject = OpenApiUtil.mapPathObjects(fileSpec, apisEntry, globalObject);
      AuthObject authObject = OpenApiUtil.getApiAuthObject(globalObject.getAuthSchemas(), pathObject);

      templateFactory.fillTemplate(filePathToSave, fileSpec, apisEntry.getKey().substring(0, 1).toUpperCase()
                                                             + apisEntry.getKey().substring(1), pathObject, authObject);

      if (fileSpec.getCallMode()) {
        addAuthentications(authObject);
      }
    }
  }

  private void addAuthentications(AuthObject authObject) {

    if (null != authObject.getSecurityRequirements() && !authObject.getSecurityRequirements().isEmpty()) {
      authObject.getSecurityRequirements().forEach(authType -> {
        if (!authentications.contains(authType)) {
          authentications.add(authType);
        }
      });

    }
  }

  private void createModelTemplate(FileSpec fileSpec, OpenAPI openAPI) {

    String fileModelToSave = processPath(fileSpec.getModelPackage(), true);
    List<String> listObjectsToCreate = OpenApiUtil.getListComponentsObjects(openAPI);
    var modelPackage = processModelPackage(fileSpec.getModelPackage());
    var basicSchemaMap = OpenApiUtil.processBasicSchemas(openAPI);
    templateFactory.setModelPackageName(modelPackage);

    listObjectsToCreate.forEach(pojoName -> {
      try {
        templateFactory.fillTemplateSchema(fileModelToSave, fileSpec.getUseLombokModelAnnotation(),
                                           OpenApiUtil.mapComponentToSchemaObject(openAPI.getComponents().getSchemas().get(pojoName),
                                                                                  pojoName, fileSpec, modelPackage));
      } catch (Exception e) {
        e.printStackTrace();
      }
      basicSchemaMap.forEach((schemaName, basicSchema) -> {
        try {
          templateFactory.fillTemplateSchema(fileModelToSave, fileSpec.getUseLombokModelAnnotation(), OpenApiUtil.mapComponentToSchemaObject(basicSchema, schemaName,
                                                                                                                                             fileSpec,
                                                                                                                                             modelPackage));
        } catch (Exception e) {
          e.printStackTrace();
        }
      });
    });
  }

  private void processPackage(String apiPackage) {
    if (StringUtils.isNotBlank(apiPackage)) {
      templateFactory.setPackageName(apiPackage.trim());
    } else if (project.getModel().getGroupId() != null) {
      templateFactory.setPackageName(project.getModel().getGroupId());
    } else {
      templateFactory.setPackageName(DEFAULT_OPENAPI_TARGET_PACKAGE);
    }
  }

  private String processModelPackage(String modelPackage) {
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

  private String processPath(String fileSpecPackage, Boolean isModel) {
    Path path;
    File[] pathList = Objects.requireNonNull(project.getBasedir().listFiles(targetFileFilter));
    if (pathList.length > 0) {
      path = pathList[0].toPath().resolve(convertPackageToTargetPath(fileSpecPackage, isModel));
    } else {
      path = project.getBasedir().toPath().resolve("target");
      path.toFile().mkdir();
      path = path.resolve(convertPackageToTargetPath(fileSpecPackage, isModel));
    }
    if (!path.toFile().isDirectory()) {
      path.toFile().mkdirs();
    }
    return path.toString();
  }

  private String convertPackageToTargetPath(String fileSpecPackage, Boolean isModel) {
    String path;

    if (StringUtils.isNotBlank(fileSpecPackage)) {
      path = GENERATED_SOURCES_PATH + fileSpecPackage.trim().replaceAll("\\.", "/");
    } else if (project.getModel().getGroupId() != null) {
      path = GENERATED_SOURCES_PATH + project.getModel().getGroupId().replaceAll("\\.", "/");
    } else {
      String pathDefault = isModel ? DEFAULT_OPENAPI_MODEL_PACKAGE : DEFAULT_OPENAPI_TARGET_PACKAGE;
      path = GENERATED_SOURCES_PATH + pathDefault.replaceAll("\\.", "/");
    }
    return path;
  }


}
