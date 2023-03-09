/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.template;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.api.generator.plugin.asyncapi.MethodObject;
import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaObject;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;

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

  private final List<ClassTemplate> schemaObjectMap = new LinkedList<>();

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

  private void fillTemplate(final String filePathToSave, final String className, final String templateName, final Map<String, Object> root) throws IOException, TemplateException {
    final File fileToSave = Paths.get(filePathToSave).normalize().toFile();
    fileToSave.mkdirs();
    final String pathToSaveMainClass = fileToSave.toPath().resolve(className + FILE_TYPE_JAVA).toString();
    fillTemplate(pathToSaveMainClass, templateName, root);
  }

  private void fillTemplate(final String pathToSaveMainClass, final String templateName, final Map<String, Object> root) throws IOException, TemplateException {
    writeTemplateToFile(templateName, root, pathToSaveMainClass);
  }

  public final void fillTemplates() throws IOException, TemplateException {
    root.put("publishMethods", publishMethods);
    root.put("subscribeMethods", subscribeMethods);
    root.put("streamBridgeMethods", streamBridgeMethods);

    if (!publishMethods.isEmpty()) {
      fillTemplate(supplierFilePath, supplierClassName, TemplateIndexConstants.TEMPLATE_API_SUPPLIERS, root);
    }

    if (!subscribeMethods.isEmpty()) {
      fillTemplate(subscribeFilePath, subscribeClassName, TemplateIndexConstants.TEMPLATE_API_CONSUMERS, root);
    }

    if (!streamBridgeMethods.isEmpty()) {
      fillTemplate(streamBridgeFilePath, streamBridgeClassName, TemplateIndexConstants.TEMPLATE_API_STREAM_BRIDGE, root);
    }

    schemaObjectMap.forEach(classTemplate -> {
      try {
        fillTemplateSchema(classTemplate, false);
      } catch (final IOException | TemplateException exception) {
        throw new FileSystemException(exception);
      }
    });

    this.generateInterfaces();
  }

  public final void fillTemplateModelClassException(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToExceptionPackage = filePathToSave.resolve("exception");
    pathToExceptionPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToExceptionPackage.resolve("ModelClassException.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MODEL_EXCEPTION, root, pathToSaveMainClass);
  }

  public final void fillTemplateNotNull(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    pathToValidatorPackage.toFile().mkdirs();
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("NotNull.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateNotNullValidator(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("NotNullValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMax(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    pathToValidatorPackage.toFile().mkdirs();
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("Max.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MAX_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMaxValidator(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("MaxValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MAX_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMin(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    pathToValidatorPackage.toFile().mkdirs();
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("Min.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MIN_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMinValidator(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("MinValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MIN_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateSize(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("Size.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_SIZE_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateSizeValidator(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("SizeValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_SIZE_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplatePattern(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    final String pathToSaveMainClass = pathToValidatorPackage.resolve("Pattern.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_PATTERN_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplatePatternValidator(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("PatternValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_PATTERN_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMultipleOf(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("exception");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("MultipleOf.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MULTIPLEOF_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMultipleOfValidator(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("MultipleOfValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MULTIPLEOF_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMaxItems(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("MaxItems.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MAX_ITEMS_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMaxItemsValidator(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("MaxItemsValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MAX_ITEMS_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMinItems(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("MinItems.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MIN_ITEMS_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateMinItemsValidator(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("MinItemsValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MIN_ITEMS_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateUniqueItems(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("UniqueItems.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_ANNOTATION, root, pathToSaveMainClass);
  }

  public final void fillTemplateUniqueItemsValidator(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveMainClass = pathToCustomValidatorPackage.resolve("UniqueItemsValidator.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_VALIDATOR_ANNOTATION, root, pathToSaveMainClass);
  }

  private void fillTemplateSchema(final ClassTemplate classTemplate, final Boolean useLombok) throws IOException, TemplateException {
    final var schemaObject = classTemplate.getClassSchema();
    final var filePath = classTemplate.getFilePath();
    if (Objects.nonNull(schemaObject) && Objects.nonNull(schemaObject.getFieldObjectList()) && !schemaObject.getFieldObjectList().isEmpty()) {
      final Map<String, Object> rootSchema = new HashMap<>();
      rootSchema.put("schema", schemaObject);
      final String templateName = null != useLombok && useLombok ? TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK : TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA;
      if (Objects.nonNull(classTemplate.getModelPackage())) {
        rootSchema.put("packageModel", classTemplate.getModelPackage());
      }
      fillTemplate(filePath.toString(), schemaObject.getClassName(), templateName, rootSchema);
      for (SchemaFieldObject fieldObject : schemaObject.getFieldObjectList()) {
        if (fieldObject.isRequired()) {
          fillTemplateNotNull(filePath.toString());
          fillTemplateNotNullValidator(filePath.toString());
        }
        if (Objects.nonNull(fieldObject.getMaximum())) {
          fillTemplateMax(filePath.toString());
          fillTemplateMaxValidator(filePath.toString());
        }
        if (Objects.nonNull(fieldObject.getMaxItems())) {
          fillTemplateMaxItems(filePath, classTemplate.getModelPackage());
          fillTemplateMaxItemsValidator(filePath, classTemplate.getModelPackage());
        }
        if (Objects.nonNull(fieldObject.getMinimum())) {
          fillTemplateMin(filePath.toString());
          fillTemplateMinValidator(filePath.toString());
        }
        if (Objects.nonNull(fieldObject.getMinItems())) {
          fillTemplateMinItems(filePath, classTemplate.getModelPackage());
          fillTemplateMinItemsValidator(filePath, classTemplate.getModelPackage());
        }
        if (Objects.nonNull(fieldObject.getMinLength()) || Objects.nonNull(fieldObject.getMaxLength())) {
          fillTemplateSize(filePath.toString());
          fillTemplateSizeValidator(filePath.toString());
        }
        if (Objects.nonNull(fieldObject.getPattern())){
          fillTemplatePattern(filePath.toString());
          fillTemplatePatternValidator(filePath, classTemplate.getModelPackage());
        }
        if (Objects.nonNull(fieldObject.getMultipleOf())){
          fillTemplateMultipleOf(filePath, classTemplate.getModelPackage());
          fillTemplateMultipleOfValidator(filePath, classTemplate.getModelPackage());
        }
        if (Objects.nonNull(fieldObject.getUniqueItems())){
          fillTemplateUniqueItems(filePath, classTemplate.getModelPackage());
          fillTemplateUniqueItemsValidator(filePath, classTemplate.getModelPackage());
        }
      }
    }
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

  public final void addSchemaObject(final String modelPackage, final String className, final SchemaObject schemaObject, final boolean usingLombok, final Path filePath) {
    schemaObjectMap.add(ClassTemplate.builder().filePath(filePath).modelPackage(modelPackage).className(className).classSchema(schemaObject).build());
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
    schemaObjectMap.clear();
    streamBridgeMethods.clear();
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

    for (MethodObject method : allMethods) {
      interfaceRoot.put("method", method);

      if (Objects.equals(method.getType(), "publish")) {
        fillTemplate(supplierFilePath, "I" + method.getOperationId().substring(0, 1).toUpperCase() + method.getOperationId().substring(1),
                     TemplateIndexConstants.TEMPLATE_INTERFACE_SUPPLIERS, interfaceRoot);
      } else if (Objects.equals(method.getType(), "subscribe")) {
        fillTemplate(subscribeFilePath, "I" + method.getOperationId().substring(0, 1).toUpperCase() + method.getOperationId().substring(1),
                     TemplateIndexConstants.TEMPLATE_INTERFACE_CONSUMERS, interfaceRoot);
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
