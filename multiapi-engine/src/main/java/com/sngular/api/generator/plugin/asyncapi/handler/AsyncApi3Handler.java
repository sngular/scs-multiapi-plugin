package com.sngular.api.generator.plugin.asyncapi.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sngular.api.generator.plugin.asyncapi.exception.*;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessBindingsResult;
import com.sngular.api.generator.plugin.asyncapi.model.ProcessMethodResult;
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.asyncapi.util.AsyncApiUtil;
import com.sngular.api.generator.plugin.asyncapi.util.BindingTypeEnum;
import com.sngular.api.generator.plugin.asyncapi.util.FactoryTypeEnum;
import com.sngular.api.generator.plugin.asyncapi.util.ReferenceProcessor;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.MapperContentUtil;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import com.sngular.api.generator.plugin.common.tools.SchemaUtil;
import freemarker.template.TemplateException;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.*;
import java.util.Map.Entry;

public class AsyncApi3Handler extends BaseAsyncApiHandler {

  public AsyncApi3Handler(final Integer springBootVersion,
                         boolean overwriteModel,
                         final File targetFolder,
                         final String processedGeneratedSourcesFolder,
                         final String groupId,
                         final File baseDir) {
    super(springBootVersion, overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, baseDir);
  }

  @Override
  public void processFileSpec(final List<SpecFile> specsListFile) {
    processedOperationIds.clear();
    templateFactory.setNotGenerateTemplate();
    for (final SpecFile fileParameter : specsListFile) {
      final FileLocation ymlParent;
      try {
        ymlParent = resolveYmlLocation(fileParameter.getFilePath());
      } catch (final IOException | URISyntaxException e) {
        throw new FileSystemException(e.getMessage());
      }
      try {
        final JsonNode openApi = AsyncApiUtil.getPojoFromSpecFile(baseDir, fileParameter);
        final JsonNode operations = openApi.get("operations");
        final Map<String, JsonNode> totalSchemas = getAllSchemas(ymlParent, openApi);
        final Iterator<Entry<String, JsonNode>> operationsIt = operations.fields();
        setUpTemplate(fileParameter, springBootVersion);
        while (operationsIt.hasNext()) {
          final Map.Entry<String, JsonNode> entry = operationsIt.next();
          final JsonNode operation = entry.getValue();
          final String operationId = entry.getKey();
          final JsonNode channel = getChannelFromOperation(openApi, operation);
          processOperation(fileParameter, ymlParent, entry, channel, operationId, operation, totalSchemas);
        }
        templateFactory.fillTemplates();
      } catch (final TemplateException | IOException e) {
        throw new FileSystemException(e);
      }
      templateFactory.clearData();
    }
  }

  @Override
  protected Map<String, JsonNode> getAllSchemas(final FileLocation ymlParent, final JsonNode node) {
    final Map<String, JsonNode> totalSchemas = new HashMap<>();
    final List<JsonNode> referenceList = node.findValues(REF);

    referenceList.forEach(reference -> {
      final ReferenceProcessor refProcessor = ReferenceProcessor.builder().ymlParent(ymlParent).totalSchemas(totalSchemas).build();
      refProcessor.processReference(node, ApiTool.getNodeAsString(reference));
    });

    ApiTool.getComponent(node, SCHEMAS).forEachRemaining(
        schema -> totalSchemas.putIfAbsent(SCHEMAS.toUpperCase() + SLASH + MapperUtil.getSchemaKey(schema.getKey()), schema.getValue())
    );

    ApiTool.getComponent(node, MESSAGES).forEachRemaining(
        message -> getMessageSchemas(message.getKey(), message.getValue(), ymlParent, totalSchemas)
    );

    getChannels(node).forEachRemaining(
        channel -> getChannelSchemas(channel.getValue(), totalSchemas, ymlParent)
    );

    return totalSchemas;
  }

  @Override
  protected void processOperation(
      final SpecFile fileParameter, final FileLocation ymlParent, final Entry<String, JsonNode> entry, final JsonNode channel,
      final String operationId, final JsonNode operation, final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException {
    final String action = ApiTool.getNodeAsString(operation, "action");
    if (!StringUtils.endsWithIgnoreCase(action,"send") && !StringUtils.endsWithIgnoreCase(action,"receive")) {
      throw new InvalidAsyncAPIException("Operation action must be either 'send' or 'receive'");
    }

    if (isValidOperation(fileParameter.getConsumer(), operationId, action, "receive", true)) {
      final var operationObject = fileParameter.getConsumer();
      operationObject.setFilePath(fileParameter.getFilePath());
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processSubscribeMethod(operationId, operation, operationObject, ymlParent, totalSchemas);
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), CONSUMER_CLASS_NAME);
    } else if (isValidOperation(fileParameter.getSupplier(), operationId, action, "send", Objects.isNull(fileParameter.getStreamBridge()))) {
      final var operationObject = fileParameter.getSupplier();
      operationObject.setFilePath(fileParameter.getFilePath());
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processSupplierMethod(operationId, operation, operationObject, ymlParent, totalSchemas);
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), SUPPLIER_CLASS_NAME);
    } else if (isValidOperation(fileParameter.getStreamBridge(), operationId, action, "send", Objects.isNull(fileParameter.getSupplier()))) {
      final var operationObject = fileParameter.getStreamBridge();
      operationObject.setFilePath(fileParameter.getFilePath());
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processStreamBridgeMethod(operationId, operation, operationObject, ymlParent, entry.getKey(), totalSchemas);
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), STREAM_BRIDGE_CLASS_NAME);
    }
  }

  @Override
  protected void processSupplierMethod(
      final String operationId, final JsonNode operation, final OperationParameterObject operationObject, final FileLocation ymlParent,
      final Map<String, JsonNode> totalSchemas) throws IOException {
    final ProcessMethodResult result = processMethod(operationId, operation, operationObject, ymlParent, totalSchemas);
    fillTemplateFactory(operationId, result, totalSchemas, operationObject);
    templateFactory.addSupplierMethod(result.getOperationId(), result.getNamespace(), result.getBindings(), result.getBindingType());
  }

  @Override
  protected void processStreamBridgeMethod(
      final String operationId, final JsonNode operation, final OperationParameterObject operationObject, final FileLocation ymlParent, final String channelName, final Map<String, JsonNode> totalSchemas)
      throws IOException {
    final ProcessMethodResult result = processMethod(operationId, operation, operationObject, ymlParent, totalSchemas);
    if (!channelName.matches("[a-zA-Z0-9.\\-]*")) {
      throw new ChannelNameException(channelName);
    }
    fillTemplateFactory(operationId, result, totalSchemas, operationObject);
    templateFactory.addStreamBridgeMethod(result.getOperationId(), result.getNamespace(), channelName, result.getBindings(), result.getBindingType());
  }

  @Override
  protected void processSubscribeMethod(final String operationId,
      final JsonNode operation, final OperationParameterObject operationObject, final FileLocation ymlParent,
      final Map<String, JsonNode> totalSchemas) throws IOException {
    final ProcessMethodResult result = processMethod(operationId, operation, operationObject, ymlParent, totalSchemas);
    fillTemplateFactory(operationId, result, totalSchemas, operationObject);
    templateFactory.addSubscribeMethod(result.getOperationId(), result.getNamespace(), result.getBindings(), result.getBindingType());
  }

  @Override
  protected void fillTemplateFactory(final String operationId,
      final ProcessMethodResult processedMethod, final Map<String, JsonNode> totalSchemas, final OperationParameterObject operationObject)
      throws IOException {
    final String classFullName = processedMethod.getNamespace();
    final String keyClassFullName = processedMethod.getBindings();
    final String modelPackage = classFullName.substring(0, classFullName.lastIndexOf("."));
    final String parentPackage = modelPackage.substring(modelPackage.lastIndexOf(".") + 1);
    final String className = classFullName.substring(classFullName.lastIndexOf(".") + 1);
    final String keyClassName = keyClassFullName != null ? keyClassFullName.substring(keyClassFullName.lastIndexOf(".") + 1) : null;
    final JsonNode schemaToBuild = processedMethod.getPayload();
    if (shouldBuild(schemaToBuild)) {
      final var schemaObjectIt =
          MapperContentUtil.mapComponentToSchemaObject(totalSchemas, className, schemaToBuild, parentPackage, operationObject, this.baseDir).iterator();

      if (schemaObjectIt.hasNext()) {
        writeSchemaObject(operationObject.isUseLombokModelAnnotation(), operationObject.getModelPackage(), keyClassName, schemaObjectIt.next());
        if (Objects.nonNull(keyClassName)) {
          templateFactory.setWrapperPackageName(operationObject.getApiPackage());
          templateFactory.fillTemplateWrapper(operationObject.getApiPackage(), classFullName, className, keyClassFullName, keyClassName);
        }
        schemaObjectIt.forEachRemaining(schemaObj -> writeSchemaObject(operationObject.isUseLombokModelAnnotation(), operationObject.getModelPackage(), null, schemaObj));
      }
    }
  }

  @Override
  protected ProcessMethodResult processMethod(
      final String operationId, final JsonNode operation, final OperationParameterObject operationObject, final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas)
      throws IOException {
    final JsonNode message = operation.get("messages").get(0);
    final Pair<String, JsonNode> payloadInfo;
    final var processBindingsResultBuilder = ProcessBindingsResult.builder();
    if (message.has(REF)) {
      payloadInfo = processMethodRef(processBindingsResultBuilder, ApiTool.getRefValue(message), operationObject, ymlParent, totalSchemas,
                                     message);
    } else if (message.has(PAYLOAD)) {
      payloadInfo = processPayload(operationObject, calculateMessageName(operationId, message), ApiTool.getNode(message, PAYLOAD), ymlParent);
      if (ApiTool.hasNode(message, BINDINGS)) {
        processBindings(processBindingsResultBuilder, message, operationObject);
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

  @Override
  protected Pair<String, JsonNode> processPayload(final OperationParameterObject operationObject, final String messageName, final JsonNode payload, final FileLocation ymlParent)
      throws IOException {
    final String namespace;
    if (ApiTool.hasRef(payload)) {
      namespace = processMessageRef(payload, operationObject.getModelPackage(), ymlParent);
    } else {
      namespace = operationObject.getModelPackage() + PACKAGE_SEPARATOR_STR + messageName;
    }
    return Pair.of(namespace, payload);
  }

  @Override
  protected Pair<String, JsonNode> processMethodRef(
      final ProcessBindingsResult.ProcessBindingsResultBuilder bindingsResult, final String messageRef, final OperationParameterObject operationObject,
      final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas, final JsonNode method) throws IOException {

    final var message = totalSchemas.get(MapperUtil.getRefSchemaKey(messageRef));
    if (ApiTool.hasNode(message, BINDINGS)) {
      processBindings(bindingsResult, message, operationObject);
    }
    return processPayload(operationObject, MapperUtil.getRefClass(method), solvePayload(message, totalSchemas, ymlParent), ymlParent);
  }

  private JsonNode solvePayload(final JsonNode message, Map<String, JsonNode> totalSchemas, FileLocation ymlParent) {
    JsonNode payloadNode;
    if (ApiTool.hasNode(message, PAYLOAD)) {
      payloadNode = ApiTool.getNode(message, PAYLOAD);
    } else if (ApiTool.hasRef(message)) {
      final var solvedMessage = SchemaUtil.solveRef(ApiTool.getRefValue(message), totalSchemas, ymlParent.getPath());
      payloadNode = solvePayload(solvedMessage, totalSchemas, ymlParent);
    } else {
      payloadNode = message;
    }
    return payloadNode;
  }

  @Override
  protected String processMessageRef(final JsonNode messageBody, final String modelPackage, final FileLocation ymlParent) throws IOException {
    final String namespace;
    final String messageContent = ApiTool.getRefValue(messageBody);
    if (messageContent.startsWith("#")) {
      namespace = processModelPackage(MapperUtil.getLongRefClass(messageBody), modelPackage);
    } else if (messageContent.contains("#") || StringUtils.endsWith(messageContent, "yml")
        || StringUtils.endsWith(messageContent, "yaml") || StringUtils.endsWith(messageContent, "json")) {
      namespace = processExternalRef(modelPackage, ymlParent, messageBody);
    } else {
      namespace = processExternalAvro(ymlParent, messageContent);
    }
    return namespace;
  }

  @Override
  protected String processExternalAvro(final FileLocation ymlParent, final String messageContent) throws IOException {
    String avroFilePath = messageContent;
    final String namespace;
    if (messageContent.startsWith(SLASH)) {
      avroFilePath = avroFilePath.replaceFirst(SLASH, "");
    } else if (messageContent.startsWith(".")) {
      avroFilePath = baseDir.toAbsolutePath() + avroFilePath.replaceFirst("\\.", "");
    }
    final InputStream avroFile = ymlParent.getFileAtLocation(avroFilePath);
    final ObjectMapper mapper = new ObjectMapper();
    try {
      final JsonNode fileTree = mapper.readTree(avroFile);
      final JsonNode avroNamespace = fileTree.get("namespace");

      if (avroNamespace == null) throw new InvalidAvroException(avroFilePath);

      namespace = avroNamespace.asText() + PACKAGE_SEPARATOR + fileTree.get("name").asText();
    } catch (final IOException e) {
      throw new FileSystemException(e);
    }
    return namespace;
  }

  @Override
  protected String processExternalRef(final String modelPackage, final FileLocation ymlParent, final JsonNode message) throws IOException {
    final String[] pathToFile = MapperUtil.splitReference(ApiTool.getRefValue(message));
    final String filePath = pathToFile[0];
    final JsonNode node = ApiTool.nodeFromFile(ymlParent, filePath, FactoryTypeEnum.YML);
    if (pathToFile.length > 1 && StringUtils.isNotEmpty(pathToFile[1])) {
      final String componentPath = pathToFile[1];

      if (node.at(MapperUtil.getPathToModel(componentPath)).has(MapperUtil.getModel(componentPath))) {
        return processModelPackage(MapperUtil.getModel(componentPath), modelPackage);
      } else {
        throw new ExternalRefComponentNotFoundException(MapperUtil.getModel(componentPath), filePath);
      }
    } else {
      return processModelPackage(MapperUtil.getNameFromFile(filePath), modelPackage);
    }
  }

  @Override
  protected void processBindings(final ProcessBindingsResult.ProcessBindingsResultBuilder bindingsResult, final JsonNode message,
      final CommonSpecFile commonSpecFile) {
    if (message.has(BINDINGS)) {
      final var bindingsNode = message.get(BINDINGS);
      if (bindingsNode.has(KAFKA)) {
        processKafkaBindings(bindingsResult, bindingsNode.get(KAFKA), commonSpecFile);
      } else {
        bindingsResult.bindingType(BindingTypeEnum.NONBINDING.getValue());
      }
    }
  }

  @Override
  protected void processKafkaBindings(final ProcessBindingsResult.ProcessBindingsResultBuilder bindingsResult, final JsonNode kafkaBindings, final CommonSpecFile specFile) {
    if (kafkaBindings.has(KEY)) {
      bindingsResult.bindings(MapperUtil.getSimpleType(ApiTool.getNode(kafkaBindings, "key"), specFile))
                    .bindingType(BindingTypeEnum.KAFKA.getValue());
    }
  }

  @Override
  protected String processModelPackage(final String extractedPackage, final String modelPackage) {
    final String processedPackage;
    if (modelPackage != null) {
      if (extractedPackage.contains(PACKAGE_SEPARATOR_STR) || extractedPackage.contains(SLASH)) {
        final var splitPackage = MapperUtil.splitName(extractedPackage);
        final var className = splitPackage[splitPackage.length - 1];
        processedPackage = modelPackage + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(className);
      } else {
        processedPackage = modelPackage + MapperUtil.capitalizeWithPrefix(extractedPackage);
      }
    } else if (extractedPackage.contains(PACKAGE_SEPARATOR_STR)) {
      final var splitPackage = MapperUtil.splitReference(extractedPackage);
      final var className = splitPackage[splitPackage.length - 1];
      processedPackage =
          StringUtils.join(PACKAGE_SEPARATOR_STR, Arrays.spliterator(splitPackage, 0, splitPackage.length)) + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(className);
    } else {
      processedPackage = DEFAULT_ASYNCAPI_MODEL_PACKAGE + MapperUtil.capitalizeWithPrefix(extractedPackage);
    }

    return processedPackage;
  }

  private Iterator<Entry<String, JsonNode>> getChannels(final JsonNode node) {
    return ApiTool.hasNode(node, CHANNELS) ? ApiTool.getFieldIterator(node, CHANNELS) : Collections.emptyIterator();
  }

  private void getMessageSchemas(
      final String messageName, final JsonNode message, final FileLocation ymlParent, final Map<String, JsonNode> totalSchemas) {
    if (ApiTool.hasNode(message, PAYLOAD)) {
      final JsonNode payload = message.get(PAYLOAD);
      if (!payload.has(REF)) {
        final String key = EVENT.toUpperCase() + SLASH + MapperUtil.getSchemaKey(calculateMessageName(messageName, message));
        totalSchemas.putIfAbsent(key, payload);
      }
    } else if (ApiTool.hasRef(message)) {
      final ReferenceProcessor refProcessor = ReferenceProcessor.builder().ymlParent(ymlParent).totalSchemas(totalSchemas).build();
      refProcessor.processReference(message, ApiTool.getRefValue(message));
    }
  }

  private String calculateMessageName(final String messageName, final JsonNode message) {
    final String finalMessageName;
    if (ApiTool.hasNode(message, "name")) {
      finalMessageName = ApiTool.getNodeAsString(message, "name");
    } else if (ApiTool.hasName(message)) {
      finalMessageName = ApiTool.getName(message);
    } else {
      finalMessageName = messageName;
    }
    return StringUtils.capitalize(finalMessageName);
  }

  private void getChannelSchemas(final JsonNode channel, final Map<String, JsonNode> totalSchemas, final FileLocation ymlParent) {
    if (ApiTool.hasNode(channel, MESSAGES)) {
      final var messagesIt = ApiTool.getFieldIterator(channel, MESSAGES);
      while (messagesIt.hasNext()) {
        final var message = messagesIt.next();
        getMessageSchemas(message.getKey(), message.getValue(), ymlParent, totalSchemas);
      }
    }
  }

  private boolean isValidOperation(
      final OperationParameterObject operation, final String operationId, final String action, final String expectedAction,
      final boolean excludingOperationExists) {
    final boolean result;
    if (operation != null) {
      final List<String> operationIds = operation.getOperationIds();
      result = operationIds.contains(operationId) || operationIds.isEmpty() && action.equals(expectedAction) && excludingOperationExists;
    } else {
      result = false;
    }
    return result;
  }

  @Override
  protected JsonNode getChannelFromOperation(final JsonNode openApi, final JsonNode operation) {
    if (operation.has("channel")) {
      final JsonNode channelRef = operation.get("channel");
      if (channelRef.has(REF)) {
        final String channelPath = ApiTool.getRefValue(channelRef);
        return ApiTool.getNode(openApi, channelPath.substring(1));
      }
    }
    throw new InvalidAsyncAPIException("Operation must have a channel reference");
  }

  @Override
  protected String getOperationId(final JsonNode operation) {
    if (!operation.has(OPERATION_ID)) {
      throw new InvalidAsyncAPIException("Operation must have an operationId");
    }
    final String operationId = operation.get(OPERATION_ID).asText();
    if (processedOperationIds.contains(operationId)) {
      throw new DuplicatedOperationException(operationId);
    }
    processedOperationIds.add(operationId);
    return operationId;
  }
}