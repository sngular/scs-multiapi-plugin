/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.PluginConstants;
import com.sngular.api.generator.plugin.common.files.FileLocationUtil;
import com.sngular.api.generator.plugin.common.loader.DependencySpecMaterializer;
import com.sngular.api.generator.plugin.common.loader.LocalRepositorySpecArtifactResolver;
import com.sngular.api.generator.plugin.common.loader.SpecArtifactResolver;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.model.SpecConventions;
import com.sngular.api.generator.plugin.common.model.SpringBootVersion;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.InlineSchemaNaming;
import com.sngular.api.generator.plugin.common.tools.MapperContentUtil;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import com.sngular.api.generator.plugin.common.tools.PathUtil;
import com.sngular.api.generator.plugin.common.tools.SchemaUtil;
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
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.lang3.StringUtils;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class OpenApiGenerator {

  private static final String SLASH = "/";

  /** Top-level field marking a root OpenAPI document, and the stem of its conventional path. */
  private static final String OPENAPI_MARKER = "openapi";

  private static final String DEFAULT_OPENAPI_API_PACKAGE = PluginConstants.DEFAULT_API_PACKAGE + ".openapi";

  private static final String DEFAULT_OPENAPI_MODEL_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".model";

  private static final String DEFAULT_OPENAPI_CLIENT_PACKAGE = DEFAULT_OPENAPI_API_PACKAGE + ".client";

  private final Boolean overwriteModel;

  private final Set<String> overwriteModelList = new HashSet<>();

  private final TemplateFactory templateFactory;

  private final String groupId;

  private final Path baseDir;

  private final List<String> authentications = new ArrayList<>();

  private final Integer springBootVersion;

  private final SpringBootVersion bootVersion;

  private final File targetFolder;

  private SpecArtifactResolver artifactResolver = new LocalRepositorySpecArtifactResolver();

  private DependencySpecMaterializer specMaterializer;

  private Boolean isWebClient = false;

  private Boolean isRestClient = false;

  public OpenApiGenerator(
      final Integer springBootVersion,
      final Boolean overwriteModel,
      final File targetFolder,
      final String processedGeneratedSourcesFolder,
      final String groupId,
      final File basedir) {
    this(SpringBootVersion.of(springBootVersion), overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, basedir);
  }

  /**
   * As the {@code Integer} constructor, taking {@code springBootVersion} as {@code MAJOR} or {@code MAJOR.MINOR} (e.g.
   * {@code "3.2"}), so features that need a later minor are only generated for projects on it.
   */
  public OpenApiGenerator(
      final String springBootVersion,
      final Boolean overwriteModel,
      final File targetFolder,
      final String processedGeneratedSourcesFolder,
      final String groupId,
      final File basedir) {
    this(SpringBootVersion.parse(springBootVersion), overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, basedir);
  }

  private OpenApiGenerator(
      final SpringBootVersion springBootVersion,
      final Boolean overwriteModel,
      final File targetFolder,
      final String processedGeneratedSourcesFolder,
      final String groupId,
      final File basedir) {
    this.overwriteModel = overwriteModel;
    this.groupId = groupId;
    this.baseDir = basedir.toPath().toAbsolutePath();
    this.templateFactory = new TemplateFactory(overwriteModel, targetFolder, processedGeneratedSourcesFolder, basedir);
    this.bootVersion = springBootVersion;
    this.springBootVersion = springBootVersion.getMajor();
    this.targetFolder = targetFolder;
  }

  /**
   * Installs the resolver used to fetch artifacts declared through {@code fromGroupId}/
   * {@code fromArtifactId}. Build-tool plugins supply their own so that private repositories,
   * mirrors and credentials configured for the build are honoured; without it only the local
   * repository is inspected.
   */
  public final void setArtifactResolver(final SpecArtifactResolver artifactResolver) {
    this.artifactResolver = Objects.requireNonNull(artifactResolver, "artifactResolver");
    this.specMaterializer = null;
  }

  public final void processFileSpec(final List<SpecFile> specsListFile) {
    for (SpecFile specFile : specsListFile) {
      final SpecFile resolvedSpecFile = resolveSpecFile(specFile);
      authentications.clear();
      processPackage(resolvedSpecFile.getApiPackage());
      processFile(resolvedSpecFile);
      createClients(resolvedSpecFile);
      templateFactory.clearData();
    }
  }

  /**
   * Replaces the configured {@code filePath} with the contract extracted from the declared
   * artifact, so the rest of the pipeline — including relative {@code $ref} resolution — reads an
   * ordinary file. Specs without dependency coordinates are returned untouched.
   */
  private SpecFile resolveSpecFile(final SpecFile specFile) {
    specFile.validateDependencyCoordinates();
    if (!specFile.usesExternalDependency()) {
      return StringUtils.isNotBlank(specFile.getFilePath())
          ? specFile
          : specFile.toBuilder().filePath(defaultFilePath(OPENAPI_MARKER, baseDir)).build();
    }
    if (Objects.isNull(specMaterializer)) {
      specMaterializer = new DependencySpecMaterializer(artifactResolver, targetFolder);
    }
    return specFile.toBuilder().filePath(specMaterializer.materialize(specFile, OPENAPI_MARKER).toString()).build();
  }

  /**
   * Applies the conventional contract location to a spec that declares no {@code filePath}. The
   * first conventional path that exists in the module wins; when none does, the preferred spelling
   * is used anyway so the failure names the file that was expected.
   */
  private String defaultFilePath(final String rootMarker, final Path moduleDir) {
    final List<String> conventional = SpecConventions.defaultFilePaths(rootMarker);
    final String chosen = conventional.stream()
                                      .filter(path -> Files.isRegularFile(moduleDir.resolve(path)))
                                      .findFirst()
                                      .orElseGet(() -> SpecConventions.defaultFilePath(rootMarker));
    log.info("No filePath configured, using the conventional location '{}'", chosen);
    return chosen;
  }

  private void processPackage(final String apiPackage) {
    if (StringUtils.isNotBlank(apiPackage)) {
      templateFactory.setPackageName(apiPackage.trim());
    } else {
      templateFactory.setPackageName(Objects.requireNonNullElse(groupId, DEFAULT_OPENAPI_API_PACKAGE));
    }
  }

  private void processFile(final SpecFile specFile) {

    final JsonNode openAPI = OpenApiUtil.getPojoFromSpecFile(baseDir, specFile);
    OpenApiUtil.mergeWebhooksIntoPaths(openAPI);

    // Determine the actual base URI for resolving external references
    final URI specBaseUri = resolveSpecBaseUri(specFile);
    OpenApiUtil.solvePathRefs(openAPI, specBaseUri);
    final String clientPackage = specFile.getClientPackage();

    validateClientOptions(specFile);
    // An HTTP service interface is backed by the consuming service's own client, so no ApiRestClient/ApiWebClient or
    // authentication classes are generated for it.
    isWebClient = specFile.isCallMode() && !specFile.isUseHttpExchange() && specFile.isReactive();
    isRestClient = specFile.isCallMode() && !specFile.isUseHttpExchange() && !specFile.isReactive();
    if (specFile.isCallMode()) {
      templateFactory.setWebClientPackageName(StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE);
      templateFactory.setAuthPackageName((StringUtils.isNotBlank(clientPackage) ? clientPackage : DEFAULT_OPENAPI_CLIENT_PACKAGE) + ".auth");
    }

    templateFactory.calculateJavaEEPackage(springBootVersion);
    templateFactory.calculateJacksonPackage(springBootVersion);
    // RestClient is part of Spring Framework 6.1, i.e. Spring Boot 3.2.
    templateFactory.setSupportsRestClient(bootVersion.isAtLeast(3, 2));
    // Resolve the model package up front so the API interface imports models from the same
    // package they are actually written to (the interface is rendered before the models).
    // Only when a package can be derived from the spec (explicit modelPackage, or apiPackage);
    // otherwise the legacy default resolution is preserved untouched.
    if (StringUtils.isNotBlank(specFile.getApiPackage()) || StringUtils.isNotBlank(specFile.getModelPackage())) {
      templateFactory.setModelPackageName(processModelPackage(specFile.getApiPackage(), specFile.getModelPackage()));
    }
    final var globalObject = createApiTemplate(specFile, openAPI);

    createModelTemplate(specFile, openAPI, globalObject);
    templateFactory.fillTemplates();
  }

  private void validateClientOptions(final SpecFile specFile) {
    if (specFile.isUseHttpExchange() && !specFile.isCallMode()) {
      throw new CodeGenerationException("useHttpExchange generates client interfaces, so it needs callMode=true (spec " + specFile.getFilePath() + ")");
    }
    if (specFile.isUseHttpExchange() && !bootVersion.isAtLeast(3, 0)) {
      throw new CodeGenerationException("useHttpExchange needs Spring Boot 3 or later (@HttpExchange is part of Spring Framework 6), but springBootVersion is "
                                        + bootVersion + " (spec " + specFile.getFilePath() + ")");
    }
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
      } catch (IOException e) {
        throw new GeneratorTemplateException("Template Generator problem", e);
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
      } catch (IOException e) {
        throw new GeneratorTemplateException("Error filling the template", specFile.getFilePath(), e);
      }

      if (specFile.isCallMode()) {
        addAuthentications(authObject);
      }
    }

    return globalObject;
  }

  private void createModelTemplate(final SpecFile specFile, final JsonNode openAPI, final GlobalObject globalObject) {
    final var modelPackage = processModelPackage(specFile.getApiPackage(), specFile.getModelPackage());

    final var totalSchemas = OpenApiUtil.processPaths(openAPI, globalObject.getSchemaMap(), specFile);
    templateFactory.setModelPackageName(modelPackage);
    InlineSchemaNaming.prepare(totalSchemas);
    try {
      processModels(specFile, modelPackage, totalSchemas, overwriteModel);
    } finally {
      InlineSchemaNaming.clear();
    }
  }

  /**
   * Resolves the actual base URI for a spec file, handling classpath resources, filesystem paths, and remote URLs.
   * This is crucial for resolving external references ($ref) correctly when the spec is loaded from a dependency JAR.
   *
   * @param specFile the spec file configuration
   * @return the base URI for resolving external references
   */
  private URI resolveSpecBaseUri(final SpecFile specFile) {
    final String filePath = specFile.getFilePath();

    if (PathUtil.isRemoteUri(filePath)) {
      return URI.create(filePath).resolve(".");
    }

    // Check if spec is in classpath (e.g., from a dependency JAR)
    try {
      final var classPathResource = OpenApiGenerator.class.getClassLoader().getResource(filePath);
      if (Objects.nonNull(classPathResource)) {
        return FileLocationUtil.getParentUri(classPathResource.toURI());
      }
    } catch (final Exception e) {
      log.debug("Spec not found in classpath, trying filesystem: {}", e.getMessage());
    }

    // Filesystem fallback: use the actual file's directory
    return baseDir.resolve(filePath).getParent().toUri();
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

  private void addAuthentications(final AuthObject authObject) {

    if (null != authObject.getSecurityRequirements() && !authObject.getSecurityRequirements().isEmpty()) {
      authObject.getSecurityRequirements().forEach(authType -> {
        if (!authentications.contains(authType)) {
          authentications.add(authType);
        }
      });
    }
  }

  private String processModelPackage(final String apiPackage, final String modelPackage) {
    final String modelReturnPackage;
    if (StringUtils.isNotBlank(modelPackage)) {
      modelReturnPackage = modelPackage.trim();
    } else if (StringUtils.isNotBlank(apiPackage)) {
      modelReturnPackage = apiPackage.trim() + ".model";
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
          processModel(specFile, modelPackage, basicSchemaMap, overwrite, chooseRightName(schemaName), basicSchema);
        }
      } else {
        processModel(specFile, modelPackage, basicSchemaMap, overwrite, chooseRightName(schemaName), basicSchema);
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
      final String refValue = ApiTool.getRefValue(basicSchema);
      final var refSchema = MapperUtil.getRefSchemaName(basicSchema, schemaName);
      JsonNode resolvedSchema = basicSchemaMap.get(refSchema);
      if (Objects.isNull(resolvedSchema) && StringUtils.isNotEmpty(refValue) && !refValue.startsWith("#")) {
        // Whole-file external $ref (no JSON-pointer fragment): the file IS the schema.
        // Resolve it from the filesystem so the model is generated.
        try {
          resolvedSchema =
              SchemaUtil.solveRef(refValue, basicSchemaMap, this.baseDir.resolve(specFile.getFilePath()).getParent().toUri());
        } catch (final Exception e) {
          resolvedSchema = null;
        }
      }
      if (Objects.nonNull(resolvedSchema)) {
        writeSchemaObject(specFile, refSchema, resolvedSchema, basicSchemaMap, modelPackage);
      }
    } else if (!ApiTool.isArray(basicSchema) && !TypeConstants.STRING.equalsIgnoreCase(ApiTool.getType(basicSchema))) {
      writeSchemaObject(specFile, schemaName, basicSchema, basicSchemaMap, modelPackage);
    }
  }

  private String chooseRightName(final String schemaName) {
    String rightName = schemaName;
    if (!StringUtils.startsWith(schemaName, "Inline")) {
      rightName = MapperUtil.getKeySchemaName(schemaName);
    }
    return rightName;
  }

  private void writeSchemaObject(
      final SpecFile specFile, final String schemaName, final JsonNode model, final Map<String, JsonNode> basicSchemaMap,
      final String modelPackage) {
    final String parentPackage = modelPackage.substring(modelPackage.lastIndexOf(".") + 1);
    final var schemaObjectIt = MapperContentUtil
                                   .mapComponentToSchemaObject(basicSchemaMap, schemaName, model, parentPackage, specFile, this.baseDir).iterator();
    // Write to the resolved model package only when it is derivable from the spec (explicit
    // modelPackage or apiPackage); otherwise keep the legacy default (raw modelPackage, which
    // the writer defaults to the plugin's base package).
    final String writeModelPackage =
        StringUtils.isNotBlank(specFile.getModelPackage()) || StringUtils.isNotBlank(specFile.getApiPackage())
            ? modelPackage : specFile.getModelPackage();
    if (schemaObjectIt.hasNext()) {
      writeSchemaObject(specFile.isUseLombokModelAnnotation(), specFile.isUsePactAnnotation(), writeModelPackage, schemaName, schemaObjectIt.next());
    }
    schemaObjectIt.forEachRemaining(schemaObj -> writeSchemaObject(specFile.isUseLombokModelAnnotation(), specFile.isUsePactAnnotation(), writeModelPackage, null, schemaObj));

  }

  private void writeSchemaObject(final boolean usingLombok, final boolean usingPact, final String modelPackageReceived, final String keyClassName,
                                 final SchemaObject schemaObject) {
    final var finalModelPackageReceived = StringUtils.defaultIfEmpty(modelPackageReceived, DEFAULT_OPENAPI_API_PACKAGE);
    final var destinationPackage = StringUtils.defaultIfEmpty(finalModelPackageReceived, DEFAULT_OPENAPI_API_PACKAGE + SLASH + schemaObject.getParentPackage());
    templateFactory.addSchemaObject(finalModelPackageReceived, keyClassName, schemaObject, destinationPackage, usingLombok, usingPact);
    templateFactory.checkRequiredOrCombinatorExists(schemaObject, usingLombok);
  }
}
