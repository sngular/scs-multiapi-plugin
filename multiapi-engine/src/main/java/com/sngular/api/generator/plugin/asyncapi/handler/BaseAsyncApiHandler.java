package com.sngular.api.generator.plugin.asyncapi.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.asyncapi.exception.DuplicateClassException;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessBindingsResult;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessMethodResult;
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.asyncapi.template.TemplateFactory;
import com.sngular.api.generator.plugin.common.files.ClasspathFileLocation;
import com.sngular.api.generator.plugin.common.files.DirectoryFileLocation;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.exception.InvalidAPIException;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;

@Slf4j
public abstract class BaseAsyncApiHandler {

  protected static final String PACKAGE_SEPARATOR_STR = ".";
  protected static final String SLASH = "/";
  protected static final Pattern PACKAGE_SEPARATOR = Pattern.compile(PACKAGE_SEPARATOR_STR);
  protected static final String DEFAULT_ASYNCAPI_API_PACKAGE = "com.sngular.api.asyncapi";
  protected static final String DEFAULT_ASYNCAPI_MODEL_PACKAGE = DEFAULT_ASYNCAPI_API_PACKAGE + ".model";
  protected static final String CONSUMER_CLASS_NAME = "Subscriber";
  protected static final String SUPPLIER_CLASS_NAME = "Producer";
  protected static final String STREAM_BRIDGE_CLASS_NAME = "StreamBridgeProducer";
  protected static final String SUBSCRIBE = "subscribe";
  protected static final String PUBLISH = "publish";
  protected static final String OPERATION_ID = "operationId";
  protected static final String AVSC = "avsc";
  protected static final String PAYLOAD = "payload";
  protected static final String REF = "$ref";
  protected static final String MESSAGES = "messages";
  protected static final String EVENT = "event";
  protected static final String MESSAGE = "message";
  protected static final String SCHEMAS = "schemas";
  protected static final String CHANNELS = "channels";
  protected static final String BINDINGS = "bindings";
  protected static final String KAFKA = "kafka";
  protected static final String KEY = "key";

  protected final List<String> processedOperationIds = new ArrayList<>();
  protected final List<String> processedClassnames = new ArrayList<>();
  protected final List<String> processedApiPackages = new ArrayList<>();
  protected final Path baseDir;
  protected final TemplateFactory templateFactory;
  protected final String groupId;
  protected final Integer springBootVersion;

  protected BaseAsyncApiHandler(final Integer springBootVersion,
                              boolean overwriteModel,
                              final File targetFolder,
                              final String processedGeneratedSourcesFolder,
                              final String groupId,
                              final File baseDir) {
    this.groupId = groupId;
    this.baseDir = baseDir.toPath().toAbsolutePath();
    this.templateFactory = new TemplateFactory(overwriteModel, targetFolder, processedGeneratedSourcesFolder, baseDir);
    this.springBootVersion = springBootVersion;
  }

  protected static FileLocation resolveYmlLocation(final String ymlFilePath) throws IOException {
    final var classPathInput = BaseAsyncApiHandler.class.getClassLoader().getResource(ymlFilePath);
    if (Objects.nonNull(classPathInput)) {
      return new ClasspathFileLocation(Path.of(classPathInput.getPath()).getParent());
    }

    final File f = new File(ymlFilePath);
    if (f.exists()) {
      return new DirectoryFileLocation(f.toPath().getParent());
    }

    throw new FileNotFoundException("Could not find YAML file: " + ymlFilePath);
  }

  public abstract void processFileSpec(final List<SpecFile> specsListFile);

  protected abstract Map<String, JsonNode> getAllSchemas(final FileLocation ymlParent, final JsonNode node);

  protected abstract void processOperation(
      final SpecFile fileParameter, final FileLocation ymlParent, final Map.Entry<String, JsonNode> entry, final JsonNode channel,
      final String operationId, final JsonNode channelPayload, final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException;

  protected abstract void processSupplierMethod(
      final String operationId, final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent,
      final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException;

  protected abstract void processStreamBridgeMethod(
      final String operationId, final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent, final String channelName, final Map<String, JsonNode> totalSchemas)
      throws IOException, TemplateException;

  protected abstract void processSubscribeMethod(
      final String operationId, final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent,
      final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException;

  protected abstract void fillTemplateFactory(final String operationId,
      final ProcessMethodResult processedMethod, final Map<String, JsonNode> totalSchemas, final OperationParameterObject operationObject)
      throws IOException;

  protected abstract ProcessMethodResult processMethod(final String operationId,
      final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas)
      throws IOException;

  protected abstract Pair<String, JsonNode> processPayload(final OperationParameterObject operationObject, final String messageName, final JsonNode payload, final FileLocation ymlParent)
      throws IOException;

  protected abstract Pair<String, JsonNode> processMethodRef(
      final ProcessBindingsResult.ProcessBindingsResultBuilder bindingsResult, final String messageRef, final OperationParameterObject operationObject,
      final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas, final JsonNode method) throws IOException;

  protected abstract String processMessageRef(final JsonNode messageBody, final String modelPackage, final FileLocation ymlParent) throws IOException;

  protected abstract String processExternalAvro(final FileLocation ymlParent, final String messageContent) throws IOException;

  protected abstract String processExternalRef(final String modelPackage, final FileLocation ymlParent, final JsonNode message) throws IOException;

  protected abstract void processBindings(final ProcessBindingsResult.ProcessBindingsResultBuilder bindingsResult, final JsonNode message,
      final CommonSpecFile commonSpecFile);

  protected abstract void processKafkaBindings(final ProcessBindingsResult.ProcessBindingsResultBuilder bindingsResult, final JsonNode kafkaBindings, final CommonSpecFile specFile);

  protected abstract String processModelPackage(final String extractedPackage, final String modelPackage);

  protected void setUpTemplate(final SpecFile fileParameter, final Integer springBootVersion) {
    processPackage(fileParameter);
    templateFactory.processFilePaths(fileParameter, DEFAULT_ASYNCAPI_API_PACKAGE);
    processClassNames(fileParameter);
    processEntitiesSuffix(fileParameter);
    processJavaEEPackage(springBootVersion);
  }

  protected void processEntitiesSuffix(final SpecFile fileParameter) {
    templateFactory.setSupplierEntitiesSuffix(fileParameter.getSupplier() != null && fileParameter.getSupplier().getModelNameSuffix() != null
                                                  ? fileParameter.getSupplier().getModelNameSuffix() : null);
    templateFactory.setStreamBridgeEntitiesSuffix(fileParameter.getStreamBridge() != null && fileParameter.getStreamBridge().getModelNameSuffix() != null
                                                      ? fileParameter.getStreamBridge().getModelNameSuffix() : null);
    templateFactory.setSubscribeEntitiesSuffix(fileParameter.getConsumer() != null && fileParameter.getConsumer().getModelNameSuffix() != null
                                                   ? fileParameter.getConsumer().getModelNameSuffix() : null);
  }

  protected void checkClassPackageDuplicate(final String className, final String apiPackage) {
    if (className != null && processedClassnames.contains(className)
        && apiPackage != null && processedApiPackages.contains(apiPackage)
        && processedClassnames.lastIndexOf(className) == processedApiPackages.lastIndexOf(apiPackage)) {
      throw new DuplicateClassException(className, apiPackage);
    }
  }

  protected void addProcessedClassesAndPackagesToGlobalVariables(final String className, final String apiPackage, final String defaultClassName) {
    processedClassnames.add(className != null ? className : defaultClassName);
    processedApiPackages.add(apiPackage != null ? apiPackage : DEFAULT_ASYNCAPI_API_PACKAGE);
  }

  protected void processClassNames(final SpecFile fileParameter) {
    templateFactory.setSupplierClassName(fileParameter.getSupplier() != null && fileParameter.getSupplier().getClassNamePostfix() != null
                                             ? fileParameter.getSupplier().getClassNamePostfix() : SUPPLIER_CLASS_NAME);
    templateFactory.setStreamBridgeClassName(fileParameter.getStreamBridge() != null && fileParameter.getStreamBridge().getClassNamePostfix() != null
                                                 ? fileParameter.getStreamBridge().getClassNamePostfix() : STREAM_BRIDGE_CLASS_NAME);
    templateFactory.setSubscribeClassName(fileParameter.getConsumer() != null && fileParameter.getConsumer().getClassNamePostfix() != null
                                              ? fileParameter.getConsumer().getClassNamePostfix() : CONSUMER_CLASS_NAME);
  }

  protected void processJavaEEPackage(final Integer springBootVersion) {
    templateFactory.calculateJavaEEPackage(springBootVersion);
  }

  protected void processPackage(final SpecFile fileParameter) {
    if (ObjectUtils.anyNotNull(fileParameter.getSupplier(), fileParameter.getStreamBridge(), fileParameter.getConsumer())) {
      templateFactory.setSupplierPackageName(evaluatePackage(fileParameter.getSupplier()));
      templateFactory.setStreamBridgePackageName(evaluatePackage(fileParameter.getStreamBridge()));
      templateFactory.setSubscribePackageName(evaluatePackage(fileParameter.getConsumer()));
    } else {
      throw new InvalidAPIException("No Configuration provided, nothing will be generated.");
    }
  }

  protected String evaluatePackage(final OperationParameterObject operation) {
    final String evaluated;
    if (operation != null && operation.getApiPackage() != null) {
      evaluated = operation.getApiPackage();
    } else {
      evaluated = Objects.requireNonNullElse(groupId, DEFAULT_ASYNCAPI_API_PACKAGE);
    }
    return evaluated;
  }

  protected boolean shouldBuild(final JsonNode schemaToBuild) {
    boolean result = Boolean.FALSE;
    if (ApiTool.hasRef(schemaToBuild)) {
      if (!ApiTool.getRefValue(schemaToBuild).contains(AVSC)) {
        result = Boolean.TRUE;
      }
    } else {
      result = Boolean.TRUE;
    }
    return result;
  }

  protected void writeSchemaObject(final boolean usingLombok, final String modelPackageReceived, final String keyClassName, final SchemaObject schemaObject) {
    final var destinationPackage = StringUtils.defaultIfEmpty(modelPackageReceived, DEFAULT_ASYNCAPI_API_PACKAGE + SLASH + schemaObject.getParentPackage());
    templateFactory.addSchemaObject(modelPackageReceived, keyClassName, schemaObject, destinationPackage, usingLombok);
    templateFactory.checkRequiredOrCombinatorExists(schemaObject, usingLombok);
  }

  protected abstract JsonNode getChannelFromOperation(final JsonNode openApi, final JsonNode operation);
  protected abstract String getOperationId(final JsonNode operation);
}