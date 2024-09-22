/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.template;

import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedBindingException;
import com.sngular.api.generator.plugin.asyncapi.model.MethodObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.asyncapi.util.BindingTypeEnum;
import com.sngular.api.generator.plugin.common.template.CommonTemplateFactory;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import freemarker.template.TemplateException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class TemplateFactory extends CommonTemplateFactory {

  private static final String SUBSCRIBE_PACKAGE = "subscribePackage";

  private static final String WRAPPER_PACKAGE = "wrapperPackage";

  private static final String SUPPLIER_PACKAGE = "supplierPackage";

  private static final String STREAM_BRIDGE_PACKAGE = "streamBridgePackage";

  private static final String SUPPLIER_ENTITIES_SUFFIX = "supplierEntitiesSuffix";

  private static final String STREAM_BRIDGE_ENTITIES_SUFFIX = "streamBridgeEntitiesSuffix";

  private static final String SUBSCRIBE_ENTITIES_SUFFIX = "subscribeEntitiesSuffix";

  private final List<MethodObject> publishMethods = new ArrayList<>();

  private final List<MethodObject> subscribeMethods = new ArrayList<>();

  private final List<MethodObject> streamBridgeMethods = new ArrayList<>();

  private String subscribeFilePath = null;

  private String supplierFilePath = null;

  private String streamBridgeFilePath = null;

  private String supplierClassName = null;

  private String streamBridgeClassName = null;

  private String subscribeClassName = null;

  public TemplateFactory(boolean enableOverwrite,
                         final File targetFolder,
                         final String processedGeneratedSourcesFolder,
                         final File baseDir) {
    super(enableOverwrite, targetFolder, processedGeneratedSourcesFolder, baseDir, new ClasspathTemplateLoader());
  }

  public final void fillTemplates() throws IOException, TemplateException {
    addToRoot("publishMethods", publishMethods);
    addToRoot("subscribeMethods", subscribeMethods);
    addToRoot("streamBridgeMethods", streamBridgeMethods);

    for (final var method : publishMethods) {
      fillTemplate(supplierFilePath, supplierClassName, checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_API_SUPPLIERS));
    }

    for (final var method : subscribeMethods) {
      fillTemplate(subscribeFilePath, subscribeClassName, checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_API_CONSUMERS));
    }

    for (final var method : streamBridgeMethods) {
      fillTemplate(streamBridgeFilePath, streamBridgeClassName, checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_API_STREAM_BRIDGE));
    }

    generateTemplates();

    this.generateInterfaces();
  }

  public final void setSubscribePackageName(final String packageName) {
    addToRoot(SUBSCRIBE_PACKAGE, packageName);
  }

  public final void setWrapperPackageName(final String packageName) {
    addToRoot(WRAPPER_PACKAGE, packageName);
  }

  public final void setSupplierPackageName(final String packageName) {
    addToRoot(SUPPLIER_PACKAGE, packageName);
  }

  public final void setStreamBridgePackageName(final String packageName) {
    addToRoot(STREAM_BRIDGE_PACKAGE, packageName);
  }

  public final void setSubscribeClassName(final String className) {
    addToRoot("subscribeClassName", className);
    this.subscribeClassName = className;
  }

  public final void setSupplierClassName(final String className) {
    addToRoot("supplierClassName", className);
    this.supplierClassName = className;
  }

  public final void setStreamBridgeClassName(final String className) {
    addToRoot("streamBridgeClassName", className);
    this.streamBridgeClassName = className;
  }

  public final void setSubscribeFilePath(final Path path) {
    this.subscribeFilePath = path.toString();
  }

  public final void setSupplierFilePath(final Path path) {
    this.supplierFilePath = path.toString();
  }

  public final void setStreamBridgeFilePath(final Path path) {
    this.streamBridgeFilePath = path.toString();
  }

  public final void addSupplierMethod(final String operationId, final String classNamespace, final String bindings, final String bindingType) {
    publishMethods.add(MethodObject
                         .builder()
                         .operationId(operationId)
                         .classNamespace(classNamespace)
                         .type("publish")
                         .keyClassNamespace(bindings)
                         .bindingType(bindingType)
                         .build());
  }

  public final void addStreamBridgeMethod(final String operationId, final String classNamespace, final String channelName, final String bindings, final String bindingType) {
    streamBridgeMethods.add(MethodObject
                              .builder()
                              .operationId(operationId)
                              .channelName(channelName)
                              .classNamespace(classNamespace)
                              .type("streamBridge")
                              .keyClassNamespace(bindings)
                              .bindingType(bindingType)
                              .build());
  }

  public final void addSubscribeMethod(final String operationId, final String classNamespace, final String bindings, final String bindingType) {
    subscribeMethods.add(MethodObject
                           .builder()
                           .operationId(operationId)
                           .classNamespace(classNamespace)
                           .type("subscribe")
                           .keyClassNamespace(bindings)
                           .bindingType(bindingType)
                           .build());
  }

  public final void setSupplierEntitiesSuffix(final String suffix) {
    addToRoot(SUPPLIER_ENTITIES_SUFFIX, suffix);
  }

  public final void setStreamBridgeEntitiesSuffix(final String suffix) {
    addToRoot(STREAM_BRIDGE_ENTITIES_SUFFIX, suffix);
  }

  public final void setSubscribeEntitiesSuffix(final String suffix) {
    addToRoot(SUBSCRIBE_ENTITIES_SUFFIX, suffix);
  }

  public final void calculateJavaEEPackage(final Integer springBootVersion) {
    if (3 <= springBootVersion) {
      addToRoot("javaEEPackage", "jakarta");
    } else {
      addToRoot("javaEEPackage", "javax");
    }
  }

  public final void clearData() {
    cleanData();
    publishMethods.clear();
    subscribeMethods.clear();
    streamBridgeMethods.clear();
  }

  public final void fillTemplateWrapper(
      final String modelPackage,
      final String classFullName,
      final String className,
      final String keyClassFullName,
      final String keyClassName
  ) throws IOException {
    final var filePath = processPath(getPath(modelPackage));
    addToRoot(Map.of(WRAPPER_PACKAGE, modelPackage,
                 "classNamespace", classFullName,
                 "className", className,
                 "keyNamespace", keyClassFullName,
                 "keyClassName", keyClassName));
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MESSAGE_WRAPPER, filePath, "MessageWrapper");
    clearData();
  }

  public void processFilePaths(final SpecFile fileParameter, final String defaultApiPackage) {
    var pathToCreate = convertPackageToTargetPath(fileParameter.getSupplier(), defaultApiPackage);
    if (Objects.nonNull(pathToCreate)) {
      setSupplierFilePath(processPath(pathToCreate));
    }
    pathToCreate = convertPackageToTargetPath(fileParameter.getStreamBridge(), defaultApiPackage);
    if (Objects.nonNull(pathToCreate)) {
      setStreamBridgeFilePath(processPath(pathToCreate));
    }
    pathToCreate = convertPackageToTargetPath(fileParameter.getConsumer(), defaultApiPackage);
    if (Objects.nonNull(pathToCreate)) {
      setSubscribeFilePath(processPath(pathToCreate));
    }
  }
  private void generateInterfaces() throws IOException, TemplateException {
    final ArrayList<MethodObject> allMethods = new ArrayList<>(subscribeMethods);
    allMethods.addAll(publishMethods);

    for (MethodObject method : allMethods) {
      addToRoot("method", method);

      if (Objects.equals(method.getType(), "publish")) {
        fillTemplate(supplierFilePath, "I" + method.getOperationId().substring(0, 1).toUpperCase() + method.getOperationId().substring(1),
                     checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_INTERFACE_SUPPLIERS));
      } else if (Objects.equals(method.getType(), "subscribe")) {
        fillTemplate(subscribeFilePath, "I" + method.getOperationId().substring(0, 1).toUpperCase() + method.getOperationId().substring(1),
                     checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_INTERFACE_CONSUMERS));
      }
    }
    cleanData();
  }

  private String checkTemplate(final String bindingType, final String defaultTemplate) {
    final String templateName;
    switch (BindingTypeEnum.valueOf(bindingType)) {
      case NONBINDING:
        templateName = defaultTemplate;
        break;
      case KAFKA:
        templateName = MapperUtil.splitName(defaultTemplate)[0] + TemplateIndexConstants.KAFKA_BINDINGS_FTLH;
        break;
      default:
        throw new NonSupportedBindingException(bindingType);
    }
    return templateName;
  }
}
