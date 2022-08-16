/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.template;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import net.coru.api.generator.plugin.asyncapi.MethodObject;

public class TemplateFactory {

  public static final String SUBSCRIBE_PACKAGE = "subscribePackage";

  public static final String SUPPLIER_PACKAGE = "supplierPackage";

  public static final String STREAM_BRIDGE_PACKAGE = "streamBridgePackage";

  public static final String SUPPLIER_ENTITIES_SUFFIX = "supplierEntitiesSuffix";

  public static final String STREAM_BRIDGE_ENTITIES_SUFFIX = "streamBridgeEntitiesSuffix";

  public static final String SUBSCRIBE_ENTITIES_SUFFIX = "subscribeEntitiesSuffix";

  public static final String FILE_TYPE_JAVA = ".java";

  private final Configuration cfg = new Configuration(Configuration.VERSION_2_3_27);

  private final Map<String, Object> root = new HashMap<>();

  private final List<MethodObject> publishMethods = new ArrayList<>();

  private final List<MethodObject> subscribeMethods = new ArrayList<>();

  private final List<MethodObject> streamBridgeMethods = new ArrayList<>();

  private String subscribeFilePath = null;

  private String supplierFilePath = null;

  private String streamBridgeFilePath = null;

  private String supplierClassName = null;

  private String streamBridgeClassName = null;

  private String subscribeClassName = null;

  public TemplateFactory() {
    cfg.setTemplateLoader(new ClasspathTemplateLoader());
    cfg.setDefaultEncoding("UTF-8");
    cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
    cfg.setLogTemplateExceptions(true);
  }

  public final void fillTemplate() throws IOException, TemplateException {
    root.put("publishMethods", publishMethods);
    root.put("subscribeMethods", subscribeMethods);
    root.put("streamBridgeMethods", streamBridgeMethods);

    File fileToSave;
    String pathToSaveMainClass;

    if (!publishMethods.isEmpty()) {
      fileToSave = new File(supplierFilePath);
      pathToSaveMainClass = fileToSave.toPath().resolve(supplierClassName + FILE_TYPE_JAVA).toString();
      writeTemplateToFile("templateSuppliers.ftlh", root, pathToSaveMainClass);
    }

    if (!subscribeMethods.isEmpty()) {
      fileToSave = new File(subscribeFilePath);
      pathToSaveMainClass = fileToSave.toPath().resolve(subscribeClassName + FILE_TYPE_JAVA).toString();
      writeTemplateToFile("templateConsumers.ftlh", root, pathToSaveMainClass);
    }

    if (!streamBridgeMethods.isEmpty()) {
      fileToSave = new File(streamBridgeFilePath);
      pathToSaveMainClass = fileToSave.toPath().resolve(streamBridgeClassName + FILE_TYPE_JAVA).toString();
      writeTemplateToFile("templateStreamBridge.ftlh", root, pathToSaveMainClass);
    }

    this.generateInterfaces();
  }

  public final void setSubscribePackageName(final String packageName) {
    root.put(SUBSCRIBE_PACKAGE, packageName);
  }

  public final void setSupplierPackageName(final String packageName) {
    root.put(SUPPLIER_PACKAGE, packageName);
  }

  public final void setStreamBridgePackageName(final String packageName) {
    root.put(STREAM_BRIDGE_PACKAGE, packageName);
  }

  public final void setSubscribeClassName(final String className) {
    root.put("subscribeClassName", className);
    this.subscribeClassName = className;
  }

  public final void setSupplierClassName(final String className) {
    root.put("supplierClassName", className);
    this.supplierClassName = className;
  }

  public final void setStreamBridgeClassName(final String className) {
    root.put("streamBridgeClassName", className);
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

  public final void addSupplierMethod(final String operationId, final String classNamespace) {
    publishMethods.add(new MethodObject(operationId, classNamespace, "publish"));
  }

  public final void addStreamBridgeMethod(final String operationId, final String classNamespace, final String channelName) {
    streamBridgeMethods.add(new MethodObject(operationId, classNamespace, "streamBridge", channelName));
  }

  public final void addSubscribeMethod(final String operationId, final String classNamespace) {
    subscribeMethods.add(new MethodObject(operationId, classNamespace, "subscribe"));
  }

  public final void setSupplierEntitiesSuffix(final String suffix) {
    root.put(SUPPLIER_ENTITIES_SUFFIX, suffix);
  }

  public final void setStreamBridgeEntitiesSuffix(final String suffix) {
    root.put(STREAM_BRIDGE_ENTITIES_SUFFIX, suffix);
  }

  public final void setSubscribeEntitiesSuffix(final String suffix) {
    root.put(SUBSCRIBE_ENTITIES_SUFFIX, suffix);
  }

  public final void clearData() {
    root.clear();
    publishMethods.clear();
    subscribeMethods.clear();
    subscribeFilePath = null;
    supplierFilePath = null;
    streamBridgeFilePath = null;
    supplierClassName = null;
    subscribeClassName = null;
    streamBridgeClassName = null;
  }

  private void generateInterfaces() throws IOException, TemplateException {
    final ArrayList<MethodObject> allMethods = new ArrayList<>(subscribeMethods);
    allMethods.addAll(publishMethods);

    final Map<String, Object> interfaceRoot = new HashMap<>();
    interfaceRoot.put(SUBSCRIBE_PACKAGE, root.get(SUBSCRIBE_PACKAGE));
    interfaceRoot.put(SUPPLIER_PACKAGE, root.get(SUPPLIER_PACKAGE));

    interfaceRoot.put(SUPPLIER_ENTITIES_SUFFIX, root.get(SUPPLIER_ENTITIES_SUFFIX));
    interfaceRoot.put(SUBSCRIBE_ENTITIES_SUFFIX, root.get(SUBSCRIBE_ENTITIES_SUFFIX));

    File fileToSave;
    String pathToSaveMainClass;

    for (MethodObject method : allMethods) {
      interfaceRoot.put("method", method);

      if (Objects.equals(method.getType(), "publish")) {
        fileToSave = new File(supplierFilePath);
        pathToSaveMainClass = fileToSave.toPath().resolve("I" + method.getOperationId().substring(0, 1).toUpperCase() + method.getOperationId().substring(1) + FILE_TYPE_JAVA)
                                        .toString();
        writeTemplateToFile("interfaceSupplier.ftlh", interfaceRoot, pathToSaveMainClass);
      } else if (Objects.equals(method.getType(), "subscribe")) {
        fileToSave = new File(subscribeFilePath);
        pathToSaveMainClass = fileToSave.toPath().resolve("I" + method.getOperationId().substring(0, 1).toUpperCase() + method.getOperationId().substring(1) + FILE_TYPE_JAVA)
                                        .toString();
        writeTemplateToFile("interfaceConsumer.ftlh", interfaceRoot, pathToSaveMainClass);
      }
    }
  }

  private void writeTemplateToFile(final String templateName, final Map<String, Object> root, final String path) throws IOException, TemplateException {
    final Template template = cfg.getTemplate(templateName);

    final FileWriter writer = new FileWriter(path);
    template.process(root, writer);
    writer.close();
  }
}
