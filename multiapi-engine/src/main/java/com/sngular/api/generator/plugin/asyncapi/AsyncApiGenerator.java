/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedSchemaException;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.asyncapi.template.TemplateFactory;
import com.sngular.api.generator.plugin.asyncapi.util.MapperContentUtil;
import com.sngular.api.generator.plugin.asyncapi.util.MapperUtil;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;

@SuppressWarnings("checkstyle:ClassDataAbstractionCoupling")
@Slf4j
public class AsyncApiGenerator {

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

  public AsyncApiGenerator(final File targetFolder, final String processedGeneratedSourcesFolder, final String groupId, final File baseDir) {

    this.groupId = groupId;
    this.processedGeneratedSourcesFolder = processedGeneratedSourcesFolder;
    this.targetFolder = targetFolder;
    this.baseDir = baseDir;
    templateFactory = new TemplateFactory();
    targetFileFilter = (dir, name) -> name.toLowerCase().contains(targetFolder.toPath().getFileName().toString());
  }

  public final void processFileSpec(final List<SpecFile> specsListFile) {

    final ObjectMapper om = new ObjectMapper(new YAMLFactory());
    generateExceptionTemplate = false;
    for (SpecFile fileParameter : specsListFile) {
      setUpTemplate(fileParameter);
      String avroFilePath = fileParameter.getFilePath();
      if (avroFilePath.startsWith(".")) {
        avroFilePath = baseDir.getAbsolutePath() + avroFilePath.replaceFirst("\\.", "");
      }
      final Path ymlParentPath = Paths.get(avroFilePath).toAbsolutePath().getParent();

      final var file = new File(fileParameter.getFilePath());
      try {
        final var node = om.readTree(file);
        final var internalNode = node.get("channels");
        final Map<String, JsonNode> totalSchemas = getAllSchemas(ymlParentPath, node);
        final Iterator<Entry<String, JsonNode>> iter = internalNode.fields();
        while (iter.hasNext()) {

          final Map.Entry<String, JsonNode> entry = iter.next();

          final JsonNode channel = entry.getValue();

          final String operationId = getOperationId(channel);
          final JsonNode channelPayload = getChannelPayload(channel);

          processOperation(fileParameter, ymlParentPath, entry, channel, operationId, channelPayload, totalSchemas);

          if (ObjectUtils.allNull(fileParameter.getConsumer(), fileParameter.getSupplier(), fileParameter.getStreamBridge())) {
            if (channel.has(SUBSCRIBE)) {
              checkClassPackageDuplicate(CONSUMER_CLASS_NAME, DEFAULT_ASYNCAPI_API_PACKAGE);
              processSubscribeMethod(channelPayload, DEFAULT_ASYNCAPI_API_PACKAGE, ymlParentPath, totalSchemas, false, "", "DTO", processPath(DEFAULT_ASYNCAPI_API_PACKAGE));
              addProcessedClassesAndPackagesToGlobalVariables(CONSUMER_CLASS_NAME, DEFAULT_ASYNCAPI_API_PACKAGE, CONSUMER_CLASS_NAME);
            } else {
              checkClassPackageDuplicate(SUPPLIER_CLASS_NAME, DEFAULT_ASYNCAPI_API_PACKAGE);
              processSupplierMethod(channelPayload, DEFAULT_ASYNCAPI_API_PACKAGE, ymlParentPath, totalSchemas, false, "", "DTO", processPath(DEFAULT_ASYNCAPI_API_PACKAGE));
              addProcessedClassesAndPackagesToGlobalVariables(SUPPLIER_CLASS_NAME, DEFAULT_ASYNCAPI_API_PACKAGE, SUPPLIER_CLASS_NAME);
            }
          }

        }
        templateFactory.fillTemplates();
        templateFactory.clearData();
      } catch (final TemplateException | IOException e) {
        e.printStackTrace();
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

  private Map<String, JsonNode> getAllSchemas(final Path ymlParentPath, final JsonNode node) {
    final Map<String, JsonNode> totalSchemas = new HashMap<>();
    final List<JsonNode> referenceList = node.findValues("$ref");
    referenceList.forEach(reference ->
        processReference(node, reference, ymlParentPath, totalSchemas)
    );
    final List<JsonNode> messagesList = node.findValues("messages");
    final List<JsonNode> schemasList = node.findValues("schemas");
    schemasList.forEach(schema -> schema.fields().forEachRemaining(fieldSchema -> totalSchemas.putIfAbsent(fieldSchema.getKey().toUpperCase(), fieldSchema.getValue())));
    messagesList.forEach(message ->
                           message.fields().forEachRemaining(fieldSchema -> {
                             if (fieldSchema.getValue().has("payload")) {
                               final var payload = fieldSchema.getValue().get("payload");
                               if (!payload.has("$ref")) {
                                 totalSchemas.put(fieldSchema.getKey().toUpperCase(), payload);
                               } else {
                                 totalSchemas.putIfAbsent(fieldSchema.getKey().toUpperCase(), payload.get("$ref"));
                               }
                             }
                           })
    );
    return totalSchemas;
  }

  private JsonNode solveRef(final Path ymlParentPath, final String componentName, final JsonNode reference, final Map<String, JsonNode> totalSchemas) throws IOException {
    final String[] pathToFile = reference.asText().split("#");
    final String filePath = pathToFile[0];
    JsonNode returnNode = reference;

    File file = new File(filePath);
    if (filePath.startsWith(PACKAGE_SEPARATOR_STR) || filePath.matches("^\\w.*$")) {
      file = ymlParentPath.resolve(file.toPath()).toFile();
    }
    if (filePath.endsWith("yml") || filePath.endsWith("json")) {
      final ObjectMapper om = new ObjectMapper(new YAMLFactory());
      final var node = om.readTree(file);
      if (Objects.nonNull(node.findValue(componentName))) {
        returnNode = node.findValue(componentName);
        checkReference(node, returnNode, ymlParentPath, totalSchemas);
      } else {
        throw new NonSupportedSchemaException(node.toPrettyString());
      }
    }
    return returnNode;
  }

  private void processReference(final JsonNode node, final JsonNode reference, final Path ymlParentPath, final Map<String, JsonNode> totalSchemas) {
    final String referenceLink = reference.asText();
    final String componentName = referenceLink.substring(referenceLink.lastIndexOf("/") + 1);
    final JsonNode component;
    try {
      if (referenceLink.toLowerCase().contains("yml") || referenceLink.toLowerCase().contains("json")) {
        component = solveRef(ymlParentPath, componentName, reference, totalSchemas);
      } else {
        component = node.findValue(componentName);
        if (Objects.nonNull(component)) {
          checkReference(node, component, ymlParentPath, totalSchemas);
        }
      }
    } catch (final IOException e) {
      throw new FileSystemException(e);
    }
    if (Objects.nonNull(component)) {
      totalSchemas.put(componentName.toUpperCase(), component);
    }
  }

  private void checkReference(final JsonNode mainNode, final JsonNode node, final Path ymlParentPath, final Map<String, JsonNode> totalSchemas) {
    final var localReferences = node.findValues("$ref");
    if (!localReferences.isEmpty()) {
      localReferences.forEach(localReference -> processReference(mainNode, localReference, ymlParentPath, totalSchemas));
    }
  }

  private void processOperation(
      final SpecFile fileParameter, final Path ymlParentPath, final Entry<String, JsonNode> entry, final JsonNode channel, final String operationId, final JsonNode channelPayload,
      final Map<String, JsonNode> totalSchemas) throws IOException, TemplateException {

    if (isValidOperation(fileParameter.getConsumer(), operationId, channel, SUBSCRIBE, true)) {
      final var operationObject = fileParameter.getConsumer();
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processSubscribeMethod(channelPayload, operationObject.getModelPackage(), ymlParentPath, totalSchemas, operationObject.isUseLombokModelAnnotation(),
                             operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix(), processPath(operationObject.getModelPackage()));
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), CONSUMER_CLASS_NAME);
    } else if (isValidOperation(fileParameter.getSupplier(), operationId, channel, PUBLISH, Objects.isNull(fileParameter.getStreamBridge()))) {
      final var operationObject = fileParameter.getSupplier();
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processSupplierMethod(channelPayload, operationObject.getModelPackage(), ymlParentPath, totalSchemas, operationObject.isUseLombokModelAnnotation(),
                            operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix(), processPath(operationObject.getModelPackage()));
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), SUPPLIER_CLASS_NAME);
    } else if (isValidOperation(fileParameter.getStreamBridge(), operationId, channel, PUBLISH, Objects.isNull(fileParameter.getSupplier()))) {
      final var operationObject = fileParameter.getStreamBridge();
      checkClassPackageDuplicate(operationObject.getClassNamePostfix(), operationObject.getApiPackage());
      processStreamBridgeMethod(channelPayload, operationObject.getModelPackage(), ymlParentPath, entry.getKey(), totalSchemas, operationObject.isUseLombokModelAnnotation(),
                                operationObject.getClassNamePostfix(), operationObject.getModelNameSuffix(), processPath(operationObject.getModelPackage()));
      addProcessedClassesAndPackagesToGlobalVariables(operationObject.getClassNamePostfix(), operationObject.getApiPackage(), STREAM_BRIDGE_CLASS_NAME);
    }
  }

  private boolean isValidOperation(
          final OperationParameterObject operation, final String operationId,
          final JsonNode channel, final String channelType, final boolean excludingOperationExists) {
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
    final String operationId;
    if (channel.has(SUBSCRIBE)) {
      operationId = channel.get(SUBSCRIBE).get(OPERATION_ID).asText();
    } else {
      operationId = channel.get(PUBLISH).get(OPERATION_ID).asText();
    }

    if (processedOperationIds.contains(operationId)) {
      throw new DuplicatedOperationException(operationId);
    } else {
      processedOperationIds.add(operationId);
    }
    return operationId;
  }

  private void setUpTemplate(final SpecFile fileParameter) {
    processPackage(fileParameter);
    processFilePaths(fileParameter);
    processClassNames(fileParameter);
    processEntitiesSuffix(fileParameter);
  }

  private void processFilePaths(final SpecFile fileParameter) {
    templateFactory.setSupplierFilePath(processPath(fileParameter.getSupplier()));
    templateFactory.setStreamBridgeFilePath(processPath(fileParameter.getStreamBridge()));
    templateFactory.setSubscribeFilePath(processPath(fileParameter.getConsumer()));
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

  private Path processPath(final OperationParameterObject operationParameter) {
    Path path;
    final File[] pathList = Objects.requireNonNull(baseDir.listFiles(targetFileFilter));
    if (pathList.length > 0) {
      path = pathList[0].toPath().resolve(convertPackageToTargetPath(operationParameter));
    } else {
      path = targetFolder.toPath();
      if (!path.toFile().exists() && !path.toFile().mkdirs()) {
        throw new FileSystemException(path.toFile().getName());
      }
      path = path.resolve(convertPackageToTargetPath(operationParameter));
    }
    if (!path.toFile().isDirectory() && !path.toFile().mkdirs()) {
      throw new FileSystemException(path.toFile().getName());
    }
    return path;
  }

  private Path processPath(final String packagePath) {
    Path path;
    final File[] pathList = Objects.requireNonNull(baseDir.listFiles(targetFileFilter));
    if (pathList.length > 0) {
      path = pathList[0].toPath().resolve(getPath(packagePath));
    } else {
      path = targetFolder.toPath();
      if (!path.toFile().exists() && !path.toFile().mkdirs()) {
        throw new FileSystemException(path.toFile().getName());
      }
      path = path.resolve(getPath(packagePath));
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
    } else if (Objects.nonNull(groupId)) {
      path = getPath(groupId);
    } else {
      path = getPath(DEFAULT_ASYNCAPI_API_PACKAGE);
    }
    return path;
  }

  private String getPath(final String pathName) {
    return processedGeneratedSourcesFolder + pathName.replace(PACKAGE_SEPARATOR_STR, "/");
  }

  private void processPackage(final SpecFile fileParameter) {
    templateFactory.setSupplierPackageName(evaluatePackage(fileParameter.getSupplier()));
    templateFactory.setStreamBridgePackageName(evaluatePackage(fileParameter.getStreamBridge()));
    templateFactory.setSubscribePackageName(evaluatePackage(fileParameter.getConsumer()));
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

  private void processSupplierMethod(final JsonNode channel, final String modelPackage, final Path ymlParentPath, final Map<String, JsonNode> totalSchemas,
      final boolean usingLombok, final String prefix, final String suffix, final Path filePath) throws IOException, TemplateException {
    final Pair<String, String> result = processMethod(channel, Objects.isNull(modelPackage) ? null : modelPackage, ymlParentPath, prefix, suffix);
    fillTemplateFactory(result.getValue(), totalSchemas, usingLombok, suffix, filePath);
    templateFactory.addSupplierMethod(result.getKey(), result.getValue());
  }

  private void processStreamBridgeMethod(final JsonNode channel, final String modelPackage, final Path ymlParentPath, final String channelName,
      final Map<String, JsonNode> totalSchemas, final boolean usingLombok, final String prefix, final String suffix, final Path path) throws IOException, TemplateException {
    final Pair<String, String> result = processMethod(channel, Objects.isNull(modelPackage) ? null : modelPackage, ymlParentPath, prefix, suffix);
    final String regex = "[a-zA-Z0-9.\\-]*";
    if (!channelName.matches(regex)) {
      throw new ChannelNameException(channelName);
    }
    fillTemplateFactory(result.getValue(), totalSchemas, usingLombok, suffix, path);
    templateFactory.addStreamBridgeMethod(result.getKey(), result.getValue(), channelName);
  }

  private void processSubscribeMethod(final JsonNode channel, final String modelPackage, final Path ymlParentPath, final Map<String, JsonNode> totalSchemas,
      final boolean usingLombok, final String prefix, final String suffix, final Path filePath) throws IOException, TemplateException {
    final Pair<String, String> result = processMethod(channel, Objects.isNull(modelPackage) ? null : modelPackage, ymlParentPath, prefix, suffix);
    fillTemplateFactory(result.getValue(), totalSchemas, usingLombok, suffix, filePath);
    templateFactory.addSubscribeMethod(result.getKey(), result.getValue());
  }

  private void fillTemplateFactory(final String classFullName, final Map<String, JsonNode> totalSchemas, final boolean usingLombok, final String classSuffix, final Path filePath)
      throws TemplateException, IOException {
    final var modelPackage = classFullName.substring(0, classFullName.lastIndexOf("."));
    final var className = classFullName.substring(classFullName.lastIndexOf(".") + 1);
    final var schemaToBuild = totalSchemas.get(className.toUpperCase());
    final var schemaObjectList = MapperContentUtil.mapComponentToSchemaObject(totalSchemas, modelPackage, className, schemaToBuild, null, classSuffix);
    schemaObjectList.forEach(schemaObject -> {
      templateFactory.addSchemaObject(modelPackage, className, schemaObject, usingLombok, filePath);
      checkRequiredOrCombinatorExists(schemaObject, usingLombok);
    });
  }

  private Pair<String, String> processMethod(final JsonNode channel, final String modelPackage, final Path ymlParentPath, final String prefix, final String suffix)
      throws IOException {
    final JsonNode message = channel.get("message");
    final String operationId = channel.get(OPERATION_ID).asText();
    String namespace = null;
    if (message.has("$ref")) {
      namespace = processMethodRef(message, modelPackage, ymlParentPath);
    } else if (message.has("payload")) {
      final var payload = message.get("payload");
      if (payload.has("$ref")) {
        namespace = processMethodRef(payload, modelPackage, ymlParentPath);
      } else {
        namespace = processModelPackage(MapperUtil.getPojoName(operationId, prefix, suffix), modelPackage);
      }
    } else {
      namespace = processModelPackage(MapperUtil.getPojoName(operationId, prefix, suffix), modelPackage);
    }
    return new MutablePair<>(operationId, namespace);
  }

  private String processMethodRef(final JsonNode messageBody, final String modelPackage, final Path ymlParentPath) throws IOException {
    final String namespace;
    final String messageContent = messageBody.get("$ref").asText();
    if (messageContent.startsWith("#")) {
      namespace = processModelPackage(MapperUtil.getRefClass(messageBody), modelPackage);
    } else if (messageContent.contains("#")) {
      namespace = processExternalRef(modelPackage, ymlParentPath, messageBody);
    } else {
      namespace = processExternalAvro(modelPackage, ymlParentPath, messageContent);
    }
    return namespace;
  }

  private String processExternalAvro(final String modelPackage, final Path ymlParentPath, final String messageContent) {
    String avroFilePath = messageContent;
    String namespace = "";
    if (messageContent.startsWith("/")) {
      avroFilePath = avroFilePath.replaceFirst("/", "");
    } else if (messageContent.startsWith(".")) {
      avroFilePath = baseDir.getAbsolutePath() + avroFilePath.replaceFirst("\\.", "");
    }
    final File avroFile = ymlParentPath.resolve(avroFilePath).toFile();
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

  private String processExternalRef(final String modelPackage, final Path ymlParentPath, final JsonNode message) throws IOException {
    final String[] pathToFile = message.get("$ref").asText().split("#");
    final String filePath = pathToFile[0];
    final String componentPath = pathToFile[1];
    final String component;
    if (componentPath.contains("/")) {
      component = componentPath.substring(componentPath.lastIndexOf("/") + 1);
    } else {
      component = componentPath;
    }
    File file = new File(filePath);
    if (filePath.startsWith(PACKAGE_SEPARATOR_STR) || filePath.matches("^\\w.*$")) {
      file = ymlParentPath.resolve(file.toPath()).toFile();
    }
    final ObjectMapper om = new ObjectMapper(new YAMLFactory());

    if (file.exists()) {
      final var node = om.readTree(file);
      if (Objects.nonNull(node.findValue(component))) {
        return processModelPackage(component, modelPackage);
      } else {
        throw new ExternalRefComponentNotFoundException(component, filePath);
      }
    } else {
      throw new FileNotFoundException("File " + filePath + " defined in the YML not found");
    }
  }

  private String processModelPackage(final String extractedPackage, final String modelPackage) {
    final String processedPackage;
    if (modelPackage != null) {
      if (extractedPackage.contains(PACKAGE_SEPARATOR_STR)) {
        final var splitPackage = extractedPackage.split("\\.");
        final var className = splitPackage[splitPackage.length - 1];
        processedPackage = modelPackage + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(className);
      } else {
        processedPackage = modelPackage + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(extractedPackage);
      }
    } else if (extractedPackage.contains(PACKAGE_SEPARATOR_STR)) {
      final var splitPackage = extractedPackage.split("\\.");
      final var className = splitPackage[splitPackage.length - 1];
      processedPackage =
        StringUtils.join(PACKAGE_SEPARATOR_STR, Arrays.spliterator(splitPackage, 0, splitPackage.length)) + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(className);
    } else {
      processedPackage = DEFAULT_ASYNCAPI_MODEL_PACKAGE + PACKAGE_SEPARATOR_STR + StringUtils.capitalize(extractedPackage);
    }

    return processedPackage;
  }
}
