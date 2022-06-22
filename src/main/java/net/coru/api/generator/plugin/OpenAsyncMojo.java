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
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import freemarker.template.TemplateException;
import net.coru.api.generator.plugin.asyncapi.exception.ChannelNameException;
import net.coru.api.generator.plugin.asyncapi.exception.DuplicateClassException;
import net.coru.api.generator.plugin.asyncapi.exception.DuplicatedOperationException;
import net.coru.api.generator.plugin.asyncapi.exception.FileSystemException;
import net.coru.api.generator.plugin.asyncapi.parameter.FileSpec;
import net.coru.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import net.coru.api.generator.plugin.asyncapi.template.TemplateFactory;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

@SuppressWarnings("checkstyle:ClassDataAbstractionCoupling")
@Mojo(name = "asyncapi-generation", defaultPhase = LifecyclePhase.GENERATE_SOURCES, requiresDependencyResolution = ResolutionScope.COMPILE)
public final class OpenAsyncMojo extends AbstractMojo {

  public static final String SUBSCRIBE = "subscribe";

  public static final String PUBLISH = "publish";

  public static final String OPERATION_ID = "operationId";

  private static final String DEFAULT_ASYNCAPI_TARGET_PACKAGE = PluginConstants.DEFAULT_TARGET_PACKAGE + ".asyncapi";

  private static final String DEFAULT_ASYNCAPI_MODEL_PACKAGE = DEFAULT_ASYNCAPI_TARGET_PACKAGE + ".model";

  private static final String CONSUMER_CLASS_NAME = "Subscriber";

  private static final String SUPPLIER_CLASS_NAME = "Producer";

  private static final String STREAM_BRIDGE_CLASS_NAME = "StreamBridgeProducer";

  @Parameter(defaultValue = "${project}", required = true, readonly = true)
  private MavenProject project;

  @Parameter(property = "fileSpecs")
  private List<FileSpec> fileSpecs;

  private TemplateFactory templateFactory;

  private final List<String> processedOperationIds = new ArrayList<>();

  private final List<String> processedClassnames = new ArrayList<>();

  private final List<String> processedTargetPackages = new ArrayList<>();

  private final FilenameFilter targetFileFilter = (dir, name) -> name.toLowerCase().contains("target");

  @Override
  public void execute() {
    templateFactory = new TemplateFactory();

    final ObjectMapper om = new ObjectMapper(new YAMLFactory());

    for (FileSpec fileParameter : fileSpecs) {
      setUpTemplate(fileParameter);
      processDuplicates(fileParameter);

      final Path ymlParentPath = Paths.get(fileParameter.getFilePath()).toAbsolutePath().getParent();

      final var file = new File(fileParameter.getFilePath());
      try {
        final var node = om.readTree(file);
        final var internalNode = node.get("channels");

        final Iterator<Map.Entry<String, JsonNode>> iter = internalNode.fields();
        while (iter.hasNext()) {

          final Map.Entry<String, JsonNode> entry = iter.next();

          final JsonNode channel = entry.getValue();

          final String operationId = getOperationId(channel);
          final JsonNode channelPayload = getChannelPayload(channel);

          if (isValidOperation(fileParameter.getConsumer(), operationId, channel, SUBSCRIBE, true)) {
            processSubscribeMethod(channelPayload, fileParameter.getConsumer().getModelPackage(), ymlParentPath);
          } else if (isValidOperation(fileParameter.getSupplier(), operationId, channel, PUBLISH, Objects.isNull(fileParameter.getStreamBridge()))) {
            processSupplierMethod(channelPayload, fileParameter.getSupplier().getModelPackage(), ymlParentPath);
          } else if (isValidOperation(fileParameter.getStreamBridge(), operationId, channel, PUBLISH, Objects.isNull(fileParameter.getSupplier()))) {
            processStreamBridgeMethod(channelPayload, fileParameter.getStreamBridge().getModelPackage(), ymlParentPath, entry.getKey());
          }

          if (ObjectUtils.allNull(fileParameter.getConsumer(), fileParameter.getSupplier(), fileParameter.getStreamBridge())) {
            if (channel.has(SUBSCRIBE)) {
              processSubscribeMethod(channelPayload, null, ymlParentPath);
            } else {
              processSupplierMethod(channelPayload, null, ymlParentPath);
            }
          }

        }
        templateFactory.fillTemplate();
        templateFactory.clearData();
      } catch (TemplateException | IOException e) {
        e.printStackTrace();
      }
    }
  }

  private boolean isValidOperation(final OperationParameterObject operation, final String operationId,
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
    JsonNode payload = null;
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

  private void setUpTemplate(final FileSpec fileParameter) {
    processPackage(fileParameter);
    processFilePaths(fileParameter);
    processClassnames(fileParameter);
    processEntitiesPostfix(fileParameter);
  }

  private void processFilePaths(final FileSpec fileParameter) {
    templateFactory.setSupplierFilePath(processPath(fileParameter.getSupplier()));
    templateFactory.setStreamBridgeFilePath(processPath(fileParameter.getStreamBridge()));
    templateFactory.setSubscribeFilePath(processPath(fileParameter.getConsumer()));
  }

  private void processEntitiesPostfix(final FileSpec fileParameter) {
    templateFactory.setSupplierEntitiesPostfix(fileParameter.getSupplier() != null && fileParameter.getSupplier().getEntitiesPostfix() != null
                                                 ? fileParameter.getSupplier().getEntitiesPostfix() : null);
    templateFactory.setStreamBridgeEntitiesPostfix(fileParameter.getStreamBridge() != null && fileParameter.getStreamBridge().getEntitiesPostfix() != null
                                                     ? fileParameter.getStreamBridge().getEntitiesPostfix() : null);
    templateFactory.setSubscribeEntitiesPostfix(fileParameter.getConsumer() != null && fileParameter.getConsumer().getEntitiesPostfix() != null
                                                  ? fileParameter.getConsumer().getEntitiesPostfix() : null);
  }

  private void processDuplicates(final FileSpec fileParameter) {
    OperationParameterObject operation;
    if (fileParameter.getConsumer() != null) {
      operation = fileParameter.getConsumer();
      checkClassPackageDuplicate(operation.getClassNamePostfix(), operation.getTargetPackage(), CONSUMER_CLASS_NAME);
    } else {
      checkClassPackageDuplicate(CONSUMER_CLASS_NAME, DEFAULT_ASYNCAPI_TARGET_PACKAGE, CONSUMER_CLASS_NAME);
    }
    if (fileParameter.getSupplier() != null) {
      operation = fileParameter.getSupplier();
      checkClassPackageDuplicate(operation.getClassNamePostfix(), operation.getTargetPackage(), SUPPLIER_CLASS_NAME);
    } else {
      checkClassPackageDuplicate(SUPPLIER_CLASS_NAME, DEFAULT_ASYNCAPI_TARGET_PACKAGE, SUPPLIER_CLASS_NAME);
    }
    if (fileParameter.getStreamBridge() != null) {
      operation = fileParameter.getStreamBridge();
      checkClassPackageDuplicate(operation.getClassNamePostfix(), operation.getTargetPackage(), STREAM_BRIDGE_CLASS_NAME);
    } else {
      checkClassPackageDuplicate(STREAM_BRIDGE_CLASS_NAME, DEFAULT_ASYNCAPI_TARGET_PACKAGE, STREAM_BRIDGE_CLASS_NAME);
    }
  }

  private void checkClassPackageDuplicate(final String className, final String targetPackage, final String defaultClassName) {
    if (className != null && processedClassnames.contains(className)
        && targetPackage != null && processedTargetPackages.contains(targetPackage)
        && processedClassnames.lastIndexOf(className) == processedTargetPackages.lastIndexOf(targetPackage)) {
      throw new DuplicateClassException(className, targetPackage);
    } else {
      processedClassnames.add(className != null ? className : defaultClassName);
      processedTargetPackages.add(targetPackage != null ? targetPackage : DEFAULT_ASYNCAPI_TARGET_PACKAGE);
    }
  }

  private void processClassnames(final FileSpec fileParameter) {
    templateFactory.setSupplierClassName(fileParameter.getSupplier() != null && fileParameter.getSupplier().getClassNamePostfix() != null
                                           ? fileParameter.getSupplier().getClassNamePostfix() : SUPPLIER_CLASS_NAME);
    templateFactory.setStreamBridgeClassName(fileParameter.getStreamBridge() != null && fileParameter.getStreamBridge().getClassNamePostfix() != null
                                               ? fileParameter.getStreamBridge().getClassNamePostfix() : STREAM_BRIDGE_CLASS_NAME);
    templateFactory.setSubscribeClassName(fileParameter.getConsumer() != null && fileParameter.getConsumer().getClassNamePostfix() != null
                                            ? fileParameter.getConsumer().getClassNamePostfix() : CONSUMER_CLASS_NAME);
  }

  private Path processPath(final OperationParameterObject operationParameter) {
    Path path;
    final File[] pathList = Objects.requireNonNull(project.getBasedir().listFiles(targetFileFilter));
    if (pathList.length > 0) {
      path = pathList[0].toPath().resolve(convertPackageToTargetPath(operationParameter));
    } else {
      path = project.getBasedir().toPath().resolve("target");
      if (!path.toFile().mkdirs()) {
        throw new FileSystemException(path.toFile().getName());
      }
      path = path.resolve(convertPackageToTargetPath(operationParameter));
    }
    if (!path.toFile().isDirectory() && !path.toFile().mkdirs()) {
      throw new FileSystemException(path.toFile().getName());
    }
    project.addCompileSourceRoot(path.toString());
    return path;
  }

  private String convertPackageToTargetPath(final OperationParameterObject operationParameter) {
    final String targetPackage = operationParameter != null ? operationParameter.getTargetPackage() : null;
    final String path;
    if (targetPackage != null) {
      path = PluginConstants.GENERATED_SOURCES_PATH + targetPackage.replace(".", "/");
    } else if (project.getModel().getGroupId() != null) {
      path = PluginConstants.GENERATED_SOURCES_PATH + project.getModel().getGroupId().replace(".", "/");
    } else {
      path = PluginConstants.GENERATED_SOURCES_PATH + DEFAULT_ASYNCAPI_TARGET_PACKAGE.replace(".", "/");
    }
    return path;
  }

  private void processPackage(final FileSpec fileParameter) {
    templateFactory.setSupplierPackageName(evaluatePackage(fileParameter.getSupplier()));
    templateFactory.setStreamBridgePackageName(evaluatePackage(fileParameter.getStreamBridge()));
    templateFactory.setSubscribePackageName(evaluatePackage(fileParameter.getConsumer()));
  }

  private String evaluatePackage(final OperationParameterObject operation) {
    final String evaluated;
    if (operation != null && operation.getTargetPackage() != null) {
      evaluated = operation.getTargetPackage();
    } else if (project.getModel().getGroupId() != null) {
      evaluated = project.getModel().getGroupId();
    } else {
      evaluated = DEFAULT_ASYNCAPI_TARGET_PACKAGE;
    }
    return evaluated;
  }

  private void processSupplierMethod(final JsonNode channel, final String modelPackage, final Path ymlParentPath) {
    final Pair<String, String> result = processMethod(channel, Objects.isNull(modelPackage) ? null : modelPackage, ymlParentPath);
    templateFactory.addSupplierMethod(result.getKey(), result.getValue());
  }

  private void processStreamBridgeMethod(final JsonNode channel, final String modelPackage, final Path ymlParentPath, final String channelName) {
    final Pair<String, String> result = processMethod(channel, Objects.isNull(modelPackage) ? null : modelPackage, ymlParentPath);
    final String regex = "[a-zA-Z0-9\\.\\-]*";
    if (!channelName.matches(regex)) {
      throw new ChannelNameException(channelName);
    }
    templateFactory.addStreamBridgeMethod(result.getKey(), result.getValue(), channelName);
  }

  private void processSubscribeMethod(final JsonNode channel, final String modelPackage, final Path ymlParentPath) {
    final Pair<String, String> result = processMethod(channel, Objects.isNull(modelPackage) ? null : modelPackage, ymlParentPath);
    templateFactory.addSubscribeMethod(result.getKey(), result.getValue());
  }

  private Pair<String, String> processMethod(final JsonNode channel, final String modelPackage, final Path ymlParentPath) {
    final JsonNode message = channel.get("message");
    final String operationId = channel.get(OPERATION_ID).asText();
    String namespace = "";
    if (message.get("$ref") != null && message.get("$ref").asText().startsWith("#")) {
      final String[] pathToObject = message.get("$ref").asText().split("/");
      namespace = processModelPackage(pathToObject[pathToObject.length - 1], modelPackage);
    } else if (message.get("$ref") != null) {
      String avroFilePath = message.get("$ref").asText();
      if (message.get("$ref").asText().startsWith("/")) {
        avroFilePath = avroFilePath.replaceFirst("/", "");
      }
      final File avroFile = ymlParentPath.resolve(avroFilePath).toFile();
      final ObjectMapper mapper = new ObjectMapper();
      try {
        final JsonNode fileTree = mapper.readTree(avroFile);
        final String fullNamespace = fileTree.get("namespace").asText() + "." + fileTree.get("name").asText();
        namespace = processModelPackage(fullNamespace, modelPackage);
      } catch (final IOException e) {
        e.printStackTrace();
      }
    } else {
      namespace = processModelPackage(message.fieldNames().next(), modelPackage);
    }
    return new MutablePair<>(operationId, namespace);
  }

  private String processModelPackage(final String extractedPackage, final String modelPackage) {
    final String processedPackage;
    if (modelPackage != null) {
      if (extractedPackage.contains(".")) {
        final var splittedPackage = extractedPackage.split("\\.");
        final var className = splittedPackage[splittedPackage.length - 1];

        processedPackage = modelPackage + "." + className;
      } else {
        processedPackage = modelPackage + "." + extractedPackage;
      }
    } else if (extractedPackage.contains(".")) {
      processedPackage = extractedPackage;
    } else {
      processedPackage = DEFAULT_ASYNCAPI_MODEL_PACKAGE + "." + extractedPackage;
    }

    return processedPackage;
  }
}
