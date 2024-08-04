/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.regex.Pattern;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.PluginConstants;
import com.sngular.api.generator.plugin.asyncapi.exception.*;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessBindingsResult;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessBindingsResult.ProcessBindingsResultBuilder;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessMethodResult;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.asyncapi.template.TemplateFactory;
import com.sngular.api.generator.plugin.asyncapi.util.BindingTypeEnum;
import com.sngular.api.generator.plugin.asyncapi.util.FactoryTypeEnum;
import com.sngular.api.generator.plugin.asyncapi.util.MapperContentUtil;
import com.sngular.api.generator.plugin.asyncapi.util.MapperUtil;
import com.sngular.api.generator.plugin.asyncapi.util.ReferenceProcessor;
import com.sngular.api.generator.plugin.common.files.ClasspathFileLocation;
import com.sngular.api.generator.plugin.common.files.DirectoryFileLocation;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import com.sngular.api.generator.plugin.common.model.TimeType;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.exception.InvalidAPIException;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

@Slf4j
public class AsyncApiGenerator {

  private static final String SLASH = "/";

  private static final String DEFAULT_ASYNCAPI_API_PACKAGE = PluginConstants.DEFAULT_API_PACKAGE + ".asyncapi";

  private static final String DEFAULT_ASYNCAPI_MODEL_PACKAGE = DEFAULT_ASYNCAPI_API_PACKAGE + ".model";

  private static final String CONSUMER_CLASS_NAME = "Subscriber";

  private static final String SUPPLIER_CLASS_NAME = "Producer";

  private static final String STREAM_BRIDGE_CLASS_NAME = "StreamBridgeProducer";

  private static final String SUBSCRIBE = "subscribe";

  private static final String PUBLISH = "publish";

  private static final String OPERATION_ID = "operationId";

  private static final String PACKAGE_SEPARATOR_STR = ".";

  public static final Pattern PACKAGE_SEPARATOR = Pattern.compile(PACKAGE_SEPARATOR_STR);

  private static final String AVSC = "avsc";

  private static final String PAYLOAD = "payload";

  private static final String REF = "$ref";

  private static final String MESSAGES = "messages";

  private static final String EVENT = "event";

  private static final String MESSAGE = "message";

  private static final String SCHEMAS = "schemas";

  private static final String CHANNELS = "channels";

  private static final String BINDINGS = "bindings";

  private static final String KAFKA = "kafka";

  private static final String KEY = "key";

  private final List<String> processedOperationIds = new ArrayList<>();

  private final List<String> processedClassnames = new ArrayList<>();

  private final List<String> processedApiPackages = new ArrayList<>();

  private final File targetFolder;

  private final File baseDir;

  private final FilenameFilter targetFileFilter;

  private final TemplateFactory templateFactory;

  private final String processedGeneratedSourcesFolder;

  private final String groupId;

  private final Integer springBootVersion;

  private boolean generateExceptionTemplate;

  public AsyncApiGenerator(final Integer springBootVersion, final File targetFolder, final String processedGeneratedSourcesFolder, final String groupId, final File baseDir) {
    this.groupId = groupId;
    this.processedGeneratedSourcesFolder = processedGeneratedSourcesFolder;
    this.targetFolder = targetFolder;
    this.baseDir = baseDir;
    this.templateFactory = new TemplateFactory();
    this.targetFileFilter = (dir, name) -> name.toLowerCase().contains(targetFolder.toPath().getFileName().toString());
    this.springBootVersion = springBootVersion;
  }

  public static Pair<InputStream, FileLocation> resolveYmlLocation(final String ymlFilePath) throws FileNotFoundException {
    final InputStream classPathInput = AsyncApiGenerator.class.getClassLoader().getResourceAsStream(ymlFilePath);

    final InputStream ymlFile;
    final FileLocation ymlParentPath;
    if (Objects.nonNull(classPathInput)) {
      ymlFile = classPathInput;
      ymlParentPath = new ClasspathFileLocation(ymlFilePath);
    } else {
      final File f = new File(ymlFilePath);
      ymlFile = new FileInputStream(f);
      ymlParentPath = new DirectoryFileLocation(f.toPath().getParent());
    }

    return new ImmutablePair<>(ymlFile, ymlParentPath);
  }

  public final void processFileSpec(final List<SpecFile> specsListFile) {
    final ObjectMapper om = new ObjectMapper(new YAMLFactory());
    processedOperationIds.clear();
    generateExceptionTemplate = false;
    for (final SpecFile fileParameter : specsListFile) {
      final Pair<InputStream, FileLocation> ymlFileAndPath;
      try {
        ymlFileAndPath = resolveYmlLocation(fileParameter.getFilePath());
      } catch (final IOException e) {
        throw new FileSystemException(e.getMessage());
      }
      final InputStream ymlFile = ymlFileAndPath.getLeft();
      final FileLocation ymlParent = ymlFileAndPath.getRight();

      try {
        final JsonNode openApi = om.readTree(ymlFile);
        final JsonNode internalNode = openApi.get(CHANNELS);
        final Map<String, JsonNode> totalSchemas = getAllSchemas(ymlParent, openApi);
        final Iterator<Entry<String, JsonNode>> iter = internalNode.fields();
        setUpTemplate(fileParameter, springBootVersion);
        while (iter.hasNext()) {

          final Map.Entry<String, JsonNode> entry = iter.next();

          final JsonNode channel = entry.getValue();

          final String operationId = getOperationId(channel);
          final JsonNode channelPayload = getChannelDefinition(channel);

          processOperation(fileParameter, ymlParent, entry, channel, operationId, channelPayload, totalSchemas);
        }

        templateFactory.fillTemplates(generateExceptionTemplate);
        templateFactory.clearData();
      } catch (final TemplateException | IOException e) {
        throw new FileSystemException(e);
      }
    }
  }

  private void checkRequiredOrCombinatorExists(final SchemaObject schema, final boolean useLombok) {
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

  private Map<String, JsonNode> getAllSchemas(final FileLocation ymlParent, final JsonNode node) {
    final Map<String, JsonNode> totalSchemas = new HashMap<>();
    final List<JsonNode> referenceList = node.findValues(REF);

    referenceList.forEach(reference -> {
      final ReferenceProcessor refProcessor = ReferenceProcessor.builder().ymlParent(ymlParent).totalSchemas(totalSchemas).build();
      refProcessor.processReference(node, ApiTool.getNodeAsString(reference));
    });

    ApiTool.getComponent(node, SCHEMAS).forEachRemaining(
        schema -> totalSchemas.putIfAbsent((SCHEMAS + SLASH + schema.getKey()).toUpperCase(), schema.getValue())
    );

    ApiTool.getComponent(node, MESSAGES).forEachRemaining(
        message -> getMessageSchemas(message.getKey(), message.getValue(), ymlParent, totalSchemas)
    );

    getChannels(node).forEachRemaining(
        channel -> getChannelSchemas(channel.getValue(), totalSchemas, ymlParent)
    );

    return totalSchemas;
  }

  private Iterator<Entry<String, JsonNode>> getChannels(final JsonNode node) {
    return ApiTool.hasNode(node, CHANNELS) ? ApiTool.getNode(node, CHANNELS).fields() : Collections.emptyIterator();
  }

  private void getMessageSchemas(
      final String messageName, final JsonNode message, final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas) {
    if (ApiTool.hasNode(message, PAYLOAD)) {
      final JsonNode payload = message.get(PAYLOAD);
      if (!payload.has(REF)) {
        final String key = (EVENT + SLASH + calculateMessageName(messageName, message)).toUpperCase();
        totalSchemas.putIfAbsent(key, payload);
      }
    } else if (ApiTool.hasRef(message)) {
      final ReferenceProcessor refProcessor = ReferenceProcessor.builder().ymlParent(ymlParent).totalSchemas(totalSchemas).build();
      refProcessor.processReference(message, ApiTool.getRefValue(message));
    }
  }

  private String calculateMessageName(final String messageName, final JsonNode message) {
    return StringUtils.defaultString(ApiTool.getName(message), messageName);
  }

  private void getChannelSchemas(final JsonNode channel, final Map<String, JsonNode> totalSchemas, final FileLocation ymlParent) {
    final List<String> options = List.of(PUBLISH, SUBSCRIBE);
    options.forEach(option -> {
      if (channel.has(option) && channel.get(option).has(MESSAGE)) {
        getMessageSchemas(null, channel.get(option).get(MESSAGE), ymlParent, totalSchemas);
      }
    });
  }

  private void processOperation(
      final SpecFile fileParameter, final FileLocation ymlParent, final Entry<String, JsonNode> entry, final JsonNode channel,
      final String operationId, final JsonNode channelPayload, final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException {
    if (isValidOperation(fileParameter.getConsumer(), operationId, channel, SUBSCRIBE, true)) {
      final var operationObject = fileParameter.getConsumer();
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processSubscribeMethod(channelPayload, operationObject, ymlParent, totalSchemas);
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), CONSUMER_CLASS_NAME);
    } else if (isValidOperation(fileParameter.getSupplier(), operationId, channel, PUBLISH, Objects.isNull(fileParameter.getStreamBridge()))) {
      final var operationObject = fileParameter.getSupplier();
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processSupplierMethod(channelPayload, operationObject, ymlParent, totalSchemas);
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), SUPPLIER_CLASS_NAME);
    } else if (isValidOperation(fileParameter.getStreamBridge(), operationId, channel, PUBLISH, Objects.isNull(fileParameter.getSupplier()))) {
      final var operationObject = fileParameter.getStreamBridge();
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processStreamBridgeMethod(channelPayload, operationObject, ymlParent, entry.getKey(), totalSchemas);
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), STREAM_BRIDGE_CLASS_NAME);
    }
  }

  private boolean isValidOperation(
      final OperationParameterObject operation, final String operationId, final JsonNode channel, final String channelType,
      final boolean excludingOperationExists) {
    final boolean result;
    if (operation != null) {
      final List<String> operationIds = operation.getOperationIds();
      result = operationIds.contains(operationId) || operationIds.isEmpty() && channel.has(channelType) && excludingOperationExists;
    } else {
      result = false;
    }
    return result;
  }

  private JsonNode getChannelDefinition(final JsonNode channel) {
    final JsonNode channelDefinition;
    if (channel.has(SUBSCRIBE)) {
      channelDefinition = channel.get(SUBSCRIBE);
    } else {
      channelDefinition = channel.get(PUBLISH);
    }
    return channelDefinition;
  }

  private String getOperationId(final JsonNode channel) {
    if (Objects.isNull(getChannelDefinition(channel).get(OPERATION_ID))) {
      throw new InvalidAsyncAPIException();
    } else {
      final String operationId = getChannelDefinition(channel).get(OPERATION_ID).asText();
      if (processedOperationIds.contains(operationId)) {
        throw new DuplicatedOperationException(operationId);
      } else {
        processedOperationIds.add(operationId);
      }
      return operationId;
    }
  }

  private void setUpTemplate(final SpecFile fileParameter, final Integer springBootVersion) {
    processPackage(fileParameter);
    processFilePaths(fileParameter);
    processClassNames(fileParameter);
    processEntitiesSuffix(fileParameter);
    processJavaEEPackage(springBootVersion);
  }

  private void processFilePaths(final SpecFile fileParameter) {
    var pathToCreate = convertPackageToTargetPath(fileParameter.getSupplier());
    if (Objects.nonNull(pathToCreate)) {
      templateFactory.setSupplierFilePath(processPath(pathToCreate));
    }
    pathToCreate = convertPackageToTargetPath(fileParameter.getStreamBridge());
    if (Objects.nonNull(pathToCreate)) {
      templateFactory.setStreamBridgeFilePath(processPath(pathToCreate));
    }
    pathToCreate = convertPackageToTargetPath(fileParameter.getConsumer());
    if (Objects.nonNull(pathToCreate)) {
      templateFactory.setSubscribeFilePath(processPath(pathToCreate));
    }
  }

  private void processEntitiesSuffix(final SpecFile fileParameter) {
    templateFactory.setSupplierEntitiesSuffix(fileParameter.getSupplier() != null && fileParameter.getSupplier().getModelNameSuffix() != null
                                                  ? fileParameter.getSupplier().getModelNameSuffix() : null);
    templateFactory.setStreamBridgeEntitiesSuffix(fileParameter.getStreamBridge() != null && fileParameter.getStreamBridge().getModelNameSuffix() != null
                                                      ? fileParameter.getStreamBridge().getModelNameSuffix() : null);
    templateFactory.setSubscribeEntitiesSuffix(fileParameter.getConsumer() != null && fileParameter.getConsumer().getModelNameSuffix() != null
                                                   ? fileParameter.getConsumer().getModelNameSuffix() : null);
  }

  private void checkClassPackageDuplicate(final String className, final String apiPackage) {
    if (className != null && processedClassnames.contains(className)
        && apiPackage != null && processedApiPackages.contains(apiPackage)
        && processedClassnames.lastIndexOf(className) == processedApiPackages.lastIndexOf(apiPackage)) {
      throw new DuplicateClassException(className, apiPackage);
    }
  }

  private void addProcessedClassesAndPackagesToGlobalVariables(final String className, final String apiPackage, final String defaultClassName) {
    processedClassnames.add(className != null ? className : defaultClassName);
    processedApiPackages.add(apiPackage != null ? apiPackage : DEFAULT_ASYNCAPI_API_PACKAGE);
  }

  private void processClassNames(final SpecFile fileParameter) {
    templateFactory.setSupplierClassName(fileParameter.getSupplier() != null && fileParameter.getSupplier().getClassNamePostfix() != null
                                             ? fileParameter.getSupplier().getClassNamePostfix() : SUPPLIER_CLASS_NAME);
    templateFactory.setStreamBridgeClassName(fileParameter.getStreamBridge() != null && fileParameter.getStreamBridge().getClassNamePostfix() != null
                                                 ? fileParameter.getStreamBridge().getClassNamePostfix() : STREAM_BRIDGE_CLASS_NAME);
    templateFactory.setSubscribeClassName(fileParameter.getConsumer() != null && fileParameter.getConsumer().getClassNamePostfix() != null
                                              ? fileParameter.getConsumer().getClassNamePostfix() : CONSUMER_CLASS_NAME);
  }

  private Path processPath(final String packagePath) {
    Path path;
    final File[] pathList = Objects.requireNonNull(baseDir.listFiles(targetFileFilter));
    if (pathList.length > 0) {
      path = pathList[0].toPath().resolve(packagePath);
    } else {
      path = targetFolder.toPath();
      if (!path.toFile().exists() && !path.toFile().mkdirs()) {
        throw new FileSystemException(path.toFile().getName());
      }
      path = path.resolve(packagePath);
    }
    if (!path.toFile().isDirectory() && !path.toFile().mkdirs()) {
      throw new FileSystemException(path.toFile().getName());
    }
    return path;
  }

  private String convertPackageToTargetPath(final OperationParameterObject operationParameter) {
    String path = null;
    if (Objects.nonNull(operationParameter)) {
      if (Objects.nonNull(operationParameter.getApiPackage())) {
        path = getPath(operationParameter.getApiPackage());
      } else {
        path = getPath(DEFAULT_ASYNCAPI_API_PACKAGE);
      }
    }
    return path;
  }

  private String getPath(final String pathName) {
    return processedGeneratedSourcesFolder + pathName.replace(PACKAGE_SEPARATOR_STR, SLASH);
  }

  private void processJavaEEPackage(final Integer springBootVersion) {
    templateFactory.calculateJavaEEPackage(springBootVersion);
  }

  private void processPackage(final SpecFile fileParameter) {
    if (ObjectUtils.anyNotNull(fileParameter.getSupplier(), fileParameter.getStreamBridge(), fileParameter.getConsumer())) {
      templateFactory.setSupplierPackageName(evaluatePackage(fileParameter.getSupplier()));
      templateFactory.setStreamBridgePackageName(evaluatePackage(fileParameter.getStreamBridge()));
      templateFactory.setSubscribePackageName(evaluatePackage(fileParameter.getConsumer()));
    } else {
      throw new InvalidAPIException("No Configuration provided, nothing will be generated.");
    }
  }

  private String evaluatePackage(final OperationParameterObject operation) {
    final String evaluated;
    if (operation != null && operation.getApiPackage() != null) {
      evaluated = operation.getApiPackage();
    } else {
      evaluated = Objects.requireNonNullElse(groupId, DEFAULT_ASYNCAPI_API_PACKAGE);
    }
    return evaluated;
  }

  private void processSupplierMethod(
      final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent,
      final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException {
    final ProcessMethodResult result = processMethod(channel, operationObject, ymlParent, totalSchemas);
    fillTemplateFactory(result, totalSchemas, operationObject);
    templateFactory.addSupplierMethod(result.getOperationId(), result.getNamespace(), result.getBindings(), result.getBindingType());
  }

  private void processStreamBridgeMethod(
      final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent, final String channelName, final Map<String, JsonNode> totalSchemas)
      throws IOException, TemplateException {
    final ProcessMethodResult result = processMethod(channel, operationObject, ymlParent, totalSchemas);
    final String regex = "[a-zA-Z0-9.\\-]*";
    if (!channelName.matches(regex)) {
      throw new ChannelNameException(channelName);
    }
    fillTemplateFactory(result, totalSchemas, operationObject);
    templateFactory.addStreamBridgeMethod(result.getOperationId(), result.getNamespace(), channelName, result.getBindings(), result.getBindingType());
  }

  private void processSubscribeMethod(
      final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent,
      final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException {
    final ProcessMethodResult result = processMethod(channel, operationObject, ymlParent, totalSchemas);
    fillTemplateFactory(result, totalSchemas, operationObject);
    templateFactory.addSubscribeMethod(result.getOperationId(), result.getNamespace(), result.getBindings(), result.getBindingType());
  }

  private void fillTemplateFactory(
      final ProcessMethodResult processedMethod, final Map<String, JsonNode> totalSchemas, final OperationParameterObject operationObject)
      throws TemplateException, IOException {
    final String classFullName = processedMethod.getNamespace();
    final String keyClassFullName = processedMethod.getBindings();
    final String modelPackage = classFullName.substring(0, classFullName.lastIndexOf("."));
    final String parentPackage = modelPackage.substring(modelPackage.lastIndexOf(".") + 1);
    final String className = classFullName.substring(classFullName.lastIndexOf(".") + 1);
    final String keyClassName = keyClassFullName != null ? keyClassFullName.substring(keyClassFullName.lastIndexOf(".") + 1) : null;
    final JsonNode schemaToBuild = processedMethod.getPayload();
    if (shouldBuild(schemaToBuild)) {
      final var schemaObjectIt =
          MapperContentUtil.mapComponentToSchemaObject(totalSchemas, className, schemaToBuild, null, operationObject.getModelNameSuffix(), parentPackage, modelPackage,
              operationObject.getFormats(), operationObject.getUseTimeType()).iterator();

      if (schemaObjectIt.hasNext()) {
        final var filePath = writeSchemaObject(operationObject.isUseLombokModelAnnotation(), operationObject.getModelPackage(), keyClassName, schemaObjectIt.next());
        if (Objects.nonNull(keyClassName)) {
          templateFactory.setWrapperPackageName(operationObject.getApiPackage());
          templateFactory.fillTemplateWrapper(processPath(getPath(operationObject.getApiPackage())),
                                              operationObject.getApiPackage(), classFullName, className, keyClassFullName, keyClassName);
        }
        schemaObjectIt.forEachRemaining(schemaObj -> writeSchemaObject(operationObject.isUseLombokModelAnnotation(), operationObject.getModelPackage(), null, schemaObj));

        if (Boolean.TRUE.equals(generateExceptionTemplate)) {
          templateFactory.fillTemplateModelClassException(filePath, modelPackage);
        }
      }
    }
  }

  private boolean shouldBuild(final JsonNode schemaToBuild) {
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

  private Path writeSchemaObject(final boolean usingLombok, final String modelPackageReceived, final String keyClassName, final SchemaObject schemaObject) {
    final var filePath = processPath(getPath(StringUtils.defaultIfEmpty(modelPackageReceived, DEFAULT_ASYNCAPI_API_PACKAGE + SLASH + schemaObject.getParentPackage())));
    final var propertiesPath = processPath(getPath(modelPackageReceived));
    templateFactory.addSchemaObject(modelPackageReceived, keyClassName, schemaObject, filePath, propertiesPath);
    checkRequiredOrCombinatorExists(schemaObject, usingLombok);
    return filePath;
  }

  private ProcessMethodResult processMethod(
      final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas)
      throws IOException {
    final JsonNode message = channel.get(MESSAGE);
    final String operationId = channel.get(OPERATION_ID).asText();
    final Pair<String, JsonNode> payloadInfo;
    final var processBindingsResultBuilder = ProcessBindingsResult.builder();
    if (message.has(REF)) {
      payloadInfo = processMethodRef(processBindingsResultBuilder, ApiTool.getRefValue(message), operationObject, ymlParent, totalSchemas,
                                     message);
    } else if (message.has(PAYLOAD)) {
      payloadInfo = processPayload(operationObject, ApiTool.getName(message), ApiTool.getNode(message, PAYLOAD), ymlParent);
      if (ApiTool.hasNode(message, BINDINGS)) {
        processBindings(processBindingsResultBuilder, operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix(), message, operationObject.getUseTimeType());
      }
    } else {
      throw new InvalidAsyncAPIException(operationId);
    }
    final var processBindingsResult = processBindingsResultBuilder.build();
    return ProcessMethodResult
               .builder()
               .operationId(operationId)
               .namespace(payloadInfo.getKey())
               .payload(payloadInfo.getValue())
               .bindings(processBindingsResult.getBindings())
               .bindingType(processBindingsResult.getBindingType())
               .build();
  }

  private Pair<String, JsonNode> processPayload(final OperationParameterObject operationObject, final String messageName, final JsonNode payload, final FileLocation ymlParent)
      throws IOException {
    final String namespace;
    if (payload.has(REF)) {
      namespace = processMessageRef(payload, operationObject.getModelPackage(), ymlParent);
    } else {
      namespace = operationObject.getModelPackage() + PACKAGE_SEPARATOR_STR + messageName;
    }
    return Pair.of(namespace, payload);
  }

  private Pair<String, JsonNode> processMethodRef(
      final ProcessBindingsResultBuilder bindingsResult, final String messageRef, final OperationParameterObject operationObject,
      final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas, final JsonNode method) throws IOException {

    final var message = totalSchemas.get(MapperUtil.buildKey(MapperUtil.splitName(messageRef)));
    if (ApiTool.hasNode(message, BINDINGS)) {
      processBindings(bindingsResult, operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix(), message, operationObject.getUseTimeType());
    }
    return processPayload(operationObject, MapperUtil.getRefClass(method), ApiTool.getNode(message, PAYLOAD), ymlParent);
  }

  private String processMessageRef(final JsonNode messageBody, final String modelPackage, final FileLocation ymlParent) throws IOException {
    final String namespace;
    final String messageContent = ApiTool.getRefValue(messageBody);
    if (messageContent.startsWith("#")) {
      namespace = processModelPackage(MapperUtil.getLongRefClass(messageBody), modelPackage);
    } else if (messageContent.contains("#")) {
      namespace = processExternalRef(modelPackage, ymlParent, messageBody);
    } else {
      namespace = processExternalAvro(modelPackage, ymlParent, messageContent);
    }
    return namespace;
  }

  private String processExternalAvro(final String modelPackage, final FileLocation ymlParent, final String messageContent) {
    String avroFilePath = messageContent;
    final String namespace;
    if (messageContent.startsWith(SLASH)) {
      avroFilePath = avroFilePath.replaceFirst(SLASH, "");
    } else if (messageContent.startsWith(".")) {
      avroFilePath = baseDir.getAbsolutePath() + avroFilePath.replaceFirst("\\.", "");
    }
    final InputStream avroFile = ymlParent.getFileAtLocation(avroFilePath);
    final ObjectMapper mapper = new ObjectMapper();
    try {
      final JsonNode fileTree = mapper.readTree(avroFile);
      final JsonNode avroNamespace = fileTree.get("namespace");

      if (avroNamespace == null) throw new InvalidAvroException(avroFilePath);

      namespace = avroNamespace.asText() + PACKAGE_SEPARATOR + fileTree.get("name").asText();;//processModelPackage(fullNamespace, avroPackage);
    } catch (final IOException e) {
      throw new FileSystemException(e);
    }
    return namespace;
  }

  private String processExternalRef(final String modelPackage, final FileLocation ymlParent, final JsonNode message) throws IOException {
    final String[] pathToFile = message.get(REF).asText().split("#");
    final String filePath = pathToFile[0];
    final String componentPath = pathToFile[1];
    final String component;
    final String[] path = MapperUtil.splitName(componentPath);
    component = path[path.length - 2] + SLASH + path[path.length - 1];

    final JsonNode node = ApiTool.nodeFromFile(ymlParent, filePath, FactoryTypeEnum.YML);
    if (Objects.nonNull(node.findValue(path[path.length - 2]).get(path[path.length - 1]))) {
      return processModelPackage(component, modelPackage);
    } else {
      throw new ExternalRefComponentNotFoundException(component, filePath);
    }
  }

  private void processBindings(final ProcessBindingsResultBuilder bindingsResult, final String prefix, final String suffix, final JsonNode message, 
      final TimeType useTimeType) {
    if (message.has(BINDINGS)) {
      final var bindingsNode = message.get(BINDINGS);
      if (bindingsNode.has(KAFKA)) {
        processKafkaBindings(bindingsResult, prefix, suffix, bindingsNode.get(KAFKA), useTimeType);
      } else {
        bindingsResult.bindingType(BindingTypeEnum.NONBINDING.getValue());
      }
    }
  }

  private void processKafkaBindings(final ProcessBindingsResultBuilder bindingsResult, final String prefix, final String suffix, final JsonNode kafkaBindings, 
      final TimeType useTimeType) {
    if (kafkaBindings.has(KEY)) {
      bindingsResult.bindings(MapperUtil.getSimpleType(ApiTool.getNode(kafkaBindings, "key"), prefix, suffix, useTimeType))
                    .bindingType(BindingTypeEnum.KAFKA.getValue());
    }
  }

  private String capitalizeWithPrefix(final String name) {
    final StringBuilder response = new StringBuilder();
    if (name.contains(SLASH)) {
      final var splitPackage = MapperUtil.splitName(name);
      for (int i = 0; i < splitPackage.length; i++) {
        response.append(PACKAGE_SEPARATOR_STR).append(i < splitPackage.length - 1 ? splitPackage[i] : StringUtils.capitalize(splitPackage[i]));
      }
    } else {
      response.append(PACKAGE_SEPARATOR_STR).append(StringUtils.capitalize(name));
    }
    return response.toString();
  }

  private String processModelPackage(final String extractedPackage, final String modelPackage) {
    final String processedPackage;
    if (modelPackage != null) {
      if (extractedPackage.contains(PACKAGE_SEPARATOR_STR) || extractedPackage.contains(SLASH)) {
        final var splitPackage = MapperUtil.splitName(extractedPackage);
        final var className = splitPackage[splitPackage.length - 1];
        processedPackage = modelPackage + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(className);
      } else {
        processedPackage = modelPackage + capitalizeWithPrefix(extractedPackage);
      }
    } else if (extractedPackage.contains(PACKAGE_SEPARATOR_STR)) {
      final var splitPackage = MapperUtil.splitName(extractedPackage);
      final var className = splitPackage[splitPackage.length - 1];
      processedPackage =
          StringUtils.join(PACKAGE_SEPARATOR_STR, Arrays.spliterator(splitPackage, 0, splitPackage.length)) + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(className);
    } else {
      processedPackage = DEFAULT_ASYNCAPI_MODEL_PACKAGE + capitalizeWithPrefix(extractedPackage);
    }

    return processedPackage;
  }
}
