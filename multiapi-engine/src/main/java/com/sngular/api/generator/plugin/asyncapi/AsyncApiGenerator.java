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
import com.sngular.api.generator.plugin.asyncapi.exception.ChannelNameException;
import com.sngular.api.generator.plugin.asyncapi.exception.DuplicateClassException;
import com.sngular.api.generator.plugin.asyncapi.exception.DuplicatedOperationException;
import com.sngular.api.generator.plugin.asyncapi.exception.ExternalRefComponentNotFoundException;
import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.asyncapi.exception.InvalidAsyncAPIException;
import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedSchemaException;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessBindingsResult;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessBindingsResult.ProcessBindingsResultBuilder;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessMethodResult;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.asyncapi.template.TemplateFactory;
import com.sngular.api.generator.plugin.asyncapi.util.BindingTypeEnum;
import com.sngular.api.generator.plugin.asyncapi.util.MapperContentUtil;
import com.sngular.api.generator.plugin.asyncapi.util.MapperUtil;
import com.sngular.api.generator.plugin.common.files.ClasspathFileLocation;
import com.sngular.api.generator.plugin.common.files.DirectoryFileLocation;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

@Slf4j
public class AsyncApiGenerator {

  private static final String YML = "yml";

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

  private static final String NAME = "name";

  private static final String REF = "$ref";

  private static final String MESSAGES = "messages";

  private static final String MESSAGE = "message";

  private static final String SCHEMAS = "schemas";

  private static final String COMPONENTS = "components";

  private static final String CHANNELS = "channels";

  private static final String JSON = "json";

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

  private boolean generateExceptionTemplate;

  private final Integer springBootVersion;

  public AsyncApiGenerator(final Integer springBootVersion, final File targetFolder, final String processedGeneratedSourcesFolder, final String groupId, final File baseDir) {
    this.groupId = groupId;
    this.processedGeneratedSourcesFolder = processedGeneratedSourcesFolder;
    this.targetFolder = targetFolder;
    this.baseDir = baseDir;
    templateFactory = new TemplateFactory();
    targetFileFilter = (dir, name) -> name.toLowerCase().contains(targetFolder.toPath().getFileName().toString());
    this.springBootVersion = springBootVersion;
  }

  public final void processFileSpec(final List<SpecFile> specsListFile) {
    final ObjectMapper om = new ObjectMapper(new YAMLFactory());
    generateExceptionTemplate = false;
    for (SpecFile fileParameter : specsListFile) {
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
          final JsonNode channelPayload = getChannelPayload(channel);

          handleMissingPublisherConsumer(fileParameter, channel, operationId);

          processOperation(fileParameter, ymlParent, entry, channel, operationId, channelPayload, totalSchemas);
        }

        templateFactory.fillTemplates(generateExceptionTemplate);
        templateFactory.clearData();
      } catch (final TemplateException | IOException e) {
        throw new FileSystemException(e);
      }
    }
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

  private void handleMissingPublisherConsumer(final SpecFile fileParameter, final JsonNode channel, final String operationId) {
    final OperationParameterObject operationParameter = OperationParameterObject
                                                            .builder()
                                                            .ids(operationId)
                                                            .apiPackage(DEFAULT_ASYNCAPI_API_PACKAGE)
                                                            .modelPackage(DEFAULT_ASYNCAPI_MODEL_PACKAGE)
                                                            .build();
    if (channel.has(SUBSCRIBE) && ObjectUtils.allNull(fileParameter.getConsumer(), fileParameter.getStreamBridge())) {
      try {
        checkClassPackageDuplicate(CONSUMER_CLASS_NAME, DEFAULT_ASYNCAPI_API_PACKAGE);
        fileParameter.setConsumer(operationParameter);
      } catch (final DuplicateClassException ignored) {
        // Don't set consumer
      }
    }

    if (channel.has(PUBLISH) && ObjectUtils.allNull(fileParameter.getSupplier(), fileParameter.getStreamBridge())) {
      try {
        checkClassPackageDuplicate(SUPPLIER_CLASS_NAME, DEFAULT_ASYNCAPI_API_PACKAGE);
        fileParameter.setSupplier(operationParameter);
      } catch (final DuplicateClassException ignored) {
        // Don't set supplier
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
    referenceList.forEach(reference -> processReference(node, reference, ymlParent, totalSchemas, referenceList));

    if (node.has(COMPONENTS)) {
      final List<JsonNode> schemasList = node.get(COMPONENTS).findValues(SCHEMAS);
      schemasList.forEach(
          schema -> schema.fields().forEachRemaining(
              fieldSchema -> totalSchemas.putIfAbsent((SCHEMAS + SLASH + fieldSchema.getKey()).toUpperCase(), fieldSchema.getValue())
          )
      );

      final List<JsonNode> messagesList = node.get(COMPONENTS).findValues(MESSAGES);
      messagesList.forEach(
          message -> getMessageSchemas(message, totalSchemas)
      );
    }

    if (node.has(CHANNELS)) {
      node.get(CHANNELS).forEach(
          channel -> getChannelSchemas(channel, totalSchemas)
      );
    }

    return totalSchemas;
  }

  private void getMessageSchemas(final JsonNode message, final Map<String, JsonNode> totalSchemas) {
    if (message.has(PAYLOAD)) {
      final JsonNode payload = message.get(PAYLOAD);
      if (!payload.has(REF)) {
        final String key = (MESSAGES + SLASH + message.get(NAME).textValue()).toUpperCase();
        totalSchemas.putIfAbsent(key, payload);
      }
    }
  }

  private void getChannelSchemas(final JsonNode channel, final Map<String, JsonNode> totalSchemas) {
    final List<String> options = List.of(PUBLISH, SUBSCRIBE);
    options.forEach(option -> {
      if (channel.has(option) && channel.get(option).has(MESSAGE)) {
        getMessageSchemas(channel.get(option).get(MESSAGE), totalSchemas);
      }
    });
  }

  private JsonNode solveRef(final FileLocation ymlParent, final String[] path, final String reference, final Map<String, JsonNode> totalSchemas) throws IOException {
    final String[] pathToFile = reference.split("#");
    final String filePath = pathToFile[0];
    JsonNode returnNode = null;

    if (filePath.endsWith(YML) || filePath.endsWith(JSON)) {
      final JsonNode node = nodeFromFile(ymlParent, filePath);
      if (node.findValue(path[path.length - 2]).has(path[path.length - 1])) {
        returnNode = node.findValue(path[path.length - 2]).get(path[path.length - 1]);
        checkReference(node, returnNode, ymlParent, totalSchemas, null);
      } else {
        throw new NonSupportedSchemaException(node.toPrettyString());
      }
    } else if (totalSchemas.containsKey((path[path.length - 2] + SLASH + path[path.length - 1]).toUpperCase())) {
      returnNode = totalSchemas.get((path[path.length - 2] + SLASH + path[path.length - 1]).toUpperCase());
    }
    return returnNode;
  }

  private JsonNode nodeFromFile(final FileLocation ymlParent, final String filePath) throws IOException {
    final InputStream file;
    if (filePath.startsWith(PACKAGE_SEPARATOR_STR) || filePath.matches("^\\w.*$")) {
      file = ymlParent.getFileAtLocation(filePath);
    } else {
      file = new FileInputStream(filePath);
    }

    final ObjectMapper om = new ObjectMapper(new YAMLFactory());
    return om.readTree(file);
  }

  private void processReference(
      final JsonNode node, final JsonNode reference, final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas,
      final List<JsonNode> referenceList) {
    final String referenceLink = reference.asText();
    final String[] path = MapperUtil.splitName(referenceLink);
    final JsonNode component;
    try {
      if (referenceLink.toLowerCase().contains(YML) || referenceLink.toLowerCase().contains(JSON)) {
        component = solveRef(ymlParent, path, referenceLink, totalSchemas);
      } else {
        if (referenceLink.toLowerCase().contains(AVSC)) {
          component = node.findValue(path[path.length - 1]);
        } else {
          component = (node.findValue(path[path.length - 2])).get(path[path.length - 1]);
        }
        if (Objects.nonNull(component)) {
          checkReference(node, component, ymlParent, totalSchemas, referenceList);
        }
      }
    } catch (final IOException e) {
      throw new FileSystemException(e);
    }
    if (Objects.nonNull(component)) {
      totalSchemas.put((path[path.length - 2] + SLASH + path[path.length - 1]).toUpperCase(), component);
    }
  }

  private void checkReference(
      final JsonNode mainNode, final JsonNode node, final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas,
      final List<JsonNode> referenceList) {
    final var localReferences = node.findValues(REF);
    if (!localReferences.isEmpty()) {
      localReferences.forEach(localReference -> processReference(mainNode, localReference, ymlParent, totalSchemas, referenceList));
    }
  }

  private void processOperation(
      final SpecFile fileParameter, final FileLocation ymlParent, final Entry<String, JsonNode> entry, final JsonNode channel, final String operationId,
      final JsonNode channelPayload, final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException {
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
      final OperationParameterObject operation, final String operationId, final JsonNode channel, final String channelType, final boolean excludingOperationExists) {
    final boolean result;
    if (operation != null) {
      final List<String> operationIds = operation.getOperationIds();
      result = operationIds.contains(operationId)
               || operationIds.isEmpty() && channel.has(channelType) && excludingOperationExists;
    } else {
      result = false;
    }
    return result;
  }

  private JsonNode getChannelPayload(final JsonNode channel) {
    final JsonNode payload;
    if (channel.has(SUBSCRIBE)) {
      payload = channel.get(SUBSCRIBE);
    } else {
      payload = channel.get(PUBLISH);
    }
    return payload;
  }

  private String getOperationId(final JsonNode channel) {
    if (Objects.isNull(getChannelPayload(channel).get(OPERATION_ID))) {
      throw new InvalidAsyncAPIException();
    } else {
      final String operationId = getChannelPayload(channel).get(OPERATION_ID).asText();
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
    templateFactory.setSupplierFilePath(processPath(convertPackageToTargetPath(fileParameter.getSupplier())));
    templateFactory.setStreamBridgeFilePath(processPath(convertPackageToTargetPath(fileParameter.getStreamBridge())));
    templateFactory.setSubscribeFilePath(processPath(convertPackageToTargetPath(fileParameter.getConsumer())));
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
    final String apiPackage = operationParameter != null ? operationParameter.getApiPackage() : null;
    final String path;
    if (Objects.nonNull(apiPackage)) {
      path = getPath(apiPackage);
    } else {
      path = getPath(DEFAULT_ASYNCAPI_API_PACKAGE);
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
    final var supplierPackageName = evaluatePackage(fileParameter.getSupplier());
    templateFactory.setSupplierPackageName(supplierPackageName);
    final var streamBridgePackageName = evaluatePackage(fileParameter.getStreamBridge());
    templateFactory.setStreamBridgePackageName(streamBridgePackageName);
    final var subscribePackageName = evaluatePackage(fileParameter.getConsumer());
    templateFactory.setSubscribePackageName(subscribePackageName);
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
    fillTemplateFactory(result.getNamespace(), result.getBindings(), totalSchemas, operationObject);
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
    fillTemplateFactory(result.getNamespace(), result.getBindings(), totalSchemas, operationObject);
    templateFactory.addStreamBridgeMethod(result.getOperationId(), result.getNamespace(), channelName, result.getBindings(), result.getBindingType());
  }

  private void processSubscribeMethod(
      final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent,
      final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException {
    final ProcessMethodResult result = processMethod(channel, operationObject, ymlParent, totalSchemas);
    fillTemplateFactory(result.getNamespace(), result.getBindings(), totalSchemas, operationObject);
    templateFactory.addSubscribeMethod(result.getOperationId(), result.getNamespace(), result.getBindings(), result.getBindingType());
  }

  private void fillTemplateFactory(
      final String classFullName, final String keyClassFullName, final Map<String, JsonNode> totalSchemas, final OperationParameterObject operationObject)
      throws TemplateException, IOException {
    final String modelPackage = classFullName.substring(0, classFullName.lastIndexOf("."));
    final String parentPackage = modelPackage.substring(modelPackage.lastIndexOf(".") + 1);
    final String className = classFullName.substring(classFullName.lastIndexOf(".") + 1);
    final String keyClassName = keyClassFullName != null ? keyClassFullName.substring(keyClassFullName.lastIndexOf(".") + 1) : null;
    final JsonNode schemaToBuild = totalSchemas.get((parentPackage + SLASH + className).toUpperCase());

    final var schemaObjectIt =
        MapperContentUtil.mapComponentToSchemaObject(totalSchemas, className, schemaToBuild, null, operationObject.getModelNameSuffix(), parentPackage).iterator();

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

  private Path writeSchemaObject(final boolean usingLombok, final String modelPackageReceived, final String keyClassName, final SchemaObject schemaObject) {
    final var filePath = processPath(getPath(StringUtils.defaultIfEmpty(modelPackageReceived, DEFAULT_ASYNCAPI_API_PACKAGE) + SLASH + schemaObject.getParentPackage()));
    templateFactory.addSchemaObject(modelPackageReceived, keyClassName, schemaObject, filePath);
    checkRequiredOrCombinatorExists(schemaObject, usingLombok);
    return filePath;
  }

  private ProcessMethodResult processMethod(
          final JsonNode channel, final OperationParameterObject operationObject, final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas)
      throws IOException {
    final JsonNode message = channel.get(MESSAGE);
    final String operationId = channel.get(OPERATION_ID).asText();
    final String namespace;
    final var processBindingsResultBuilder = ProcessBindingsResult.builder();
    if (message.has(REF)) {
      namespace = processMethodRef(processBindingsResultBuilder, ApiTool.getRefValue(message), operationObject, ymlParent, totalSchemas,
                                   message);
    } else if (message.has(PAYLOAD)) {
      namespace = processPayload(operationObject.getModelPackage(), ApiTool.getName(message), ApiTool.getNode(message, PAYLOAD), ymlParent,
                                 operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix(), processBindingsResultBuilder);
      if (ApiTool.hasNode(message, BINDINGS)) {
        processBindings(processBindingsResultBuilder, operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix(), message);
      }
    } else {
      namespace = processModelPackage(
              MapperUtil.getPojoName(operationId, operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix()),
              operationObject.getModelPackage());
    }
    final var processBindingsResult = processBindingsResultBuilder.build();
    return ProcessMethodResult
            .builder()
            .operationId(operationId)
            .namespace(namespace)
            .bindings(processBindingsResult.getBindings())
            .bindingType(processBindingsResult.getBindingType())
            .build();
  }

  private String processPayload(
      final String modelPackage, final String messageName,
      final JsonNode payload, final FileLocation ymlParent, final String prefix, final String suffix, final ProcessBindingsResultBuilder processBindingsResultBuilder)
      throws IOException {
    final String namespace;
    if (payload.has(REF)) {
      namespace = processMessageRef(payload, modelPackage, ymlParent);
    } else {
      namespace = modelPackage + PACKAGE_SEPARATOR_STR + MESSAGES + PACKAGE_SEPARATOR_STR + messageName;
    }
    if (payload.has(BINDINGS)) {
      processBindings(processBindingsResultBuilder, prefix, suffix, payload);
    }
    return namespace;
  }

  private String processMethodRef(final ProcessBindingsResultBuilder bindingsResult, final String messageRef, final OperationParameterObject operationObject,
      final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas, final JsonNode method) throws IOException {

    final var message = solveRef(ymlParent, MapperUtil.splitName(messageRef), ApiTool.getRefValue(method), totalSchemas);
    if (ApiTool.hasNode(message, BINDINGS)) {
      processBindings(bindingsResult, operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix(), message);
    }
    processPayload(operationObject.getModelPackage(), MapperUtil.getRefClass(method), ApiTool.getNode(message, PAYLOAD), ymlParent, operationObject.getClassNamePostfix(),
                   operationObject.getModelNameSuffix(), bindingsResult);
    return operationObject.getModelPackage() + PACKAGE_SEPARATOR_STR + MESSAGES + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(MapperUtil.getRefClass(method));
  }

  private String processMessageRef(final JsonNode messageBody, final String modelPackage, final FileLocation ymlParent) throws IOException {
    final String namespace;
    final String messageContent = messageBody.get(REF).asText();
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
    String namespace = "";
    if (messageContent.startsWith(SLASH)) {
      avroFilePath = avroFilePath.replaceFirst(SLASH, "");
    } else if (messageContent.startsWith(".")) {
      avroFilePath = baseDir.getAbsolutePath() + avroFilePath.replaceFirst("\\.", "");
    }
    final InputStream avroFile = ymlParent.getFileAtLocation(avroFilePath);
    final ObjectMapper mapper = new ObjectMapper();
    try {
      final JsonNode fileTree = mapper.readTree(avroFile);
      final String fullNamespace = fileTree.get("namespace").asText() + PACKAGE_SEPARATOR + fileTree.get("name").asText();
      namespace = processModelPackage(fullNamespace, modelPackage);
    } catch (final IOException e) {
      e.printStackTrace();
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

    final JsonNode node = nodeFromFile(ymlParent, filePath);
    if (Objects.nonNull(node.findValue(path[path.length - 2]).get(path[path.length - 1]))) {
      return processModelPackage(component, modelPackage);
    } else {
      throw new ExternalRefComponentNotFoundException(component, filePath);
    }
  }

  private void processBindings(final ProcessBindingsResult.ProcessBindingsResultBuilder bindingsResult, final String prefix, final String suffix, final JsonNode message) {
    if (message.has(BINDINGS)) {
      final var bindingsNode = message.get(BINDINGS);
      if (bindingsNode.has(KAFKA)) {
        processKafkaBindings(bindingsResult, prefix, suffix, bindingsNode.get(KAFKA));
      } else {
        bindingsResult.bindingType(BindingTypeEnum.NONBINDING.getValue());
      }
    }
  }

  private void processKafkaBindings(final ProcessBindingsResultBuilder bindingsResult, final String prefix, final String suffix, final JsonNode kafkaBindings) {
    if (kafkaBindings.has(KEY)) {
      bindingsResult.bindings(MapperUtil.getSimpleType(ApiTool.getNode(kafkaBindings, "key"), prefix, suffix))
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
      if (extractedPackage.contains(PACKAGE_SEPARATOR_STR)) {
        final var splitPackage = MapperUtil.splitName(extractedPackage);
        final var className = splitPackage[splitPackage.length - 1];
        processedPackage = modelPackage + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(className);
      } else {
        processedPackage =
            modelPackage + capitalizeWithPrefix(extractedPackage);
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
