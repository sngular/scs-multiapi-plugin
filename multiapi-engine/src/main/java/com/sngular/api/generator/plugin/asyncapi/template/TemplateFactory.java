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
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedBindingException;
import com.sngular.api.generator.plugin.asyncapi.model.MethodObject;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaObject;
import com.sngular.api.generator.plugin.asyncapi.util.BindingTypeEnum;
import com.sngular.api.generator.plugin.asyncapi.util.MapperUtil;
import com.sngular.api.generator.plugin.exception.GeneratorTemplateException;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;

public class TemplateFactory {

  public static final String SUBSCRIBE_PACKAGE = "subscribePackage";

  public static final String WRAPPER_PACKAGE = "wrapperPackage";

  public static final String SUPPLIER_PACKAGE = "supplierPackage";

  public static final String STREAM_BRIDGE_PACKAGE = "streamBridgePackage";

  public static final String SUPPLIER_ENTITIES_SUFFIX = "supplierEntitiesSuffix";

  public static final String STREAM_BRIDGE_ENTITIES_SUFFIX = "streamBridgeEntitiesSuffix";

  public static final String SUBSCRIBE_ENTITIES_SUFFIX = "subscribeEntitiesSuffix";

  public static final String FILE_TYPE_JAVA = ".java";

  public static final String EXCEPTION_PACKAGE = "exceptionPackage";

  private final Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);

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

  public final void fillTemplates(final boolean generateExceptionTemplate) throws IOException, TemplateException {
    root.put("publishMethods", publishMethods);
    root.put("subscribeMethods", subscribeMethods);
    root.put("streamBridgeMethods", streamBridgeMethods);

    for (final var method : publishMethods) {
      fillTemplate(supplierFilePath, supplierClassName, checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_API_SUPPLIERS), root);
    }

    for (final var method : subscribeMethods) {
      fillTemplate(subscribeFilePath, subscribeClassName, checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_API_CONSUMERS), root);
    }

    for (final var method : streamBridgeMethods) {
      fillTemplate(streamBridgeFilePath, streamBridgeClassName, checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_API_STREAM_BRIDGE), root);
    }

    final String exceptionPackage;
    if (Boolean.TRUE.equals(generateExceptionTemplate)) {
      exceptionPackage = getClassTemplate().getModelPackage();
    } else {
      exceptionPackage = null;
    }

    final HashSet<String> propertiesSet = new HashSet<>();
    schemaObjectMap.forEach(classTemplate -> {
      try {
        propertiesSet.addAll(fillTemplateSchema(classTemplate, false, exceptionPackage));
      } catch (final IOException | TemplateException exception) {
        throw new FileSystemException(exception);
      }
    });

    if (!schemaObjectMap.isEmpty()) {
      try {
        fillTemplates(schemaObjectMap.get(0).getPropertiesPath(), schemaObjectMap.get(0).getModelPackage(), propertiesSet);
      } catch (IOException | TemplateException e) {
        throw new GeneratorTemplateException("Generation Error", e);
      }
    }
    this.generateInterfaces();
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private void fillTemplates(final Path filePathToSave, final String modelPackage, final Set<String> fieldProperties) throws TemplateException, IOException {
    for (final String current : fieldProperties) {
      switch (current) {
        case "Size":
          fillTemplateCustom(filePathToSave, modelPackage, "Size.java", TemplateIndexConstants.TEMPLATE_SIZE_ANNOTATION, "SizeValidator.java",
                             TemplateIndexConstants.TEMPLATE_SIZE_VALIDATOR_ANNOTATION);
          break;
        case "Pattern":
          fillTemplateCustom(filePathToSave, modelPackage, "Pattern.java", TemplateIndexConstants.TEMPLATE_PATTERN_ANNOTATION,
                             "PatternValidator.java", TemplateIndexConstants.TEMPLATE_PATTERN_VALIDATOR_ANNOTATION);
          break;
        case "MultipleOf":
          fillTemplateCustom(filePathToSave, modelPackage, "MultipleOf.java", TemplateIndexConstants.TEMPLATE_MULTIPLEOF_ANNOTATION,
                             "MultipleOfValidator.java", TemplateIndexConstants.TEMPLATE_MULTIPLEOF_VALIDATOR_ANNOTATION);
          break;
        case "Maximum":
          fillTemplateCustom(filePathToSave, modelPackage, "Max.java", TemplateIndexConstants.TEMPLATE_MAX_ANNOTATION,
                             "MaxValidator.java", TemplateIndexConstants.TEMPLATE_MAX_VALIDATOR_ANNOTATION);
          break;
        case "Minimum":
          fillTemplateCustom(filePathToSave, modelPackage, "Min.java", TemplateIndexConstants.TEMPLATE_MIN_ANNOTATION,
                             "MinValidator.java", TemplateIndexConstants.TEMPLATE_MIN_VALIDATOR_ANNOTATION);
          break;
        case "MaxItems":
          fillTemplateCustom(filePathToSave, modelPackage, "MaxItems.java", TemplateIndexConstants.TEMPLATE_MAX_ITEMS_ANNOTATION,
                             "MaxItemsValidator.java", TemplateIndexConstants.TEMPLATE_MAX_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "MinItems":
          fillTemplateCustom(filePathToSave, modelPackage, "MinItems.java", TemplateIndexConstants.TEMPLATE_MIN_ITEMS_ANNOTATION,
                             "MinItemsValidator.java", TemplateIndexConstants.TEMPLATE_MIN_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "NotNull":
          fillTemplateCustom(filePathToSave, modelPackage, "NotNull.java", TemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION,
                             "NotNullValidator.java", TemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION);
          break;
        case "UniqueItems":
          fillTemplateCustom(filePathToSave, modelPackage, "UniqueItems.java", TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_ANNOTATION,
                             "UniqueItemsValidator.java", TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_VALIDATOR_ANNOTATION);
          break;
        default:
          break;
      }
    }
  }

  private ClassTemplate getClassTemplate() {
    ClassTemplate ourClassTemplate = null;
    for (ClassTemplate classTemplate : schemaObjectMap) {
      if (classTemplate.getFilePath().endsWith("schemas")) {
        ourClassTemplate = classTemplate;
        break;
      }
    }
    if (ourClassTemplate == null) {
      ourClassTemplate = schemaObjectMap.get(0);
    }

    return ourClassTemplate;
  }

  public final void fillTemplateModelClassException(final Path filePathToSave, final String modelPackage) throws IOException, TemplateException {
    final Path pathToExceptionPackage = filePathToSave.resolve("exception");
    pathToExceptionPackage.toFile().mkdirs();
    root.put(EXCEPTION_PACKAGE, modelPackage);
    final String pathToSaveMainClass = pathToExceptionPackage.resolve("ModelClassException.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MODEL_EXCEPTION, root, pathToSaveMainClass);
  }

  public final void fillTemplateCustom(
      final Path filePathToSave, final String modelPackage, final String fileNameAnnotation, final String templateAnnotation,
      final String fileNameValidator, final String templateValidator) throws TemplateException, IOException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveAnnotationClass = pathToCustomValidatorPackage.resolve(fileNameAnnotation).toString();
    writeTemplateToFile(templateAnnotation, root, pathToSaveAnnotationClass);
    final String pathToSaveValidatorClass = pathToCustomValidatorPackage.resolve(fileNameValidator).toString();
    writeTemplateToFile(templateValidator, root, pathToSaveValidatorClass);
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private Set<String> fillTemplateSchema(final ClassTemplate classTemplate, final Boolean useLombok, final String exceptionPackage)
      throws IOException, TemplateException {
    final var propertiesSet = new HashSet<String>();
    final var schemaObject = classTemplate.getClassSchema();
    final var filePath = classTemplate.getFilePath();
    if (Objects.nonNull(schemaObject) && Objects.nonNull(schemaObject.getFieldObjectList()) && !schemaObject.getFieldObjectList().isEmpty()) {
      final Map<String, Object> rootSchema = new HashMap<>();
      rootSchema.put("schema", schemaObject);
      root.put("schema", schemaObject);
      final String templateName = null != useLombok && useLombok ? TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK : TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA;
      if (Objects.nonNull(classTemplate.getModelPackage())) {
        rootSchema.put("packageModel", classTemplate.getModelPackage());
      }
      if (Objects.nonNull(exceptionPackage)) {
        rootSchema.put(EXCEPTION_PACKAGE, exceptionPackage);
        root.put(EXCEPTION_PACKAGE, exceptionPackage);
      }
      fillTemplate(filePath.toString(), schemaObject.getClassName(), templateName, rootSchema);
      for (SchemaFieldObject fieldObject : schemaObject.getFieldObjectList()) {
        propertiesSet.addAll(fieldObject.getRestrictions().getProperties());
        if (fieldObject.isRequired() && Boolean.FALSE.equals(useLombok)) {
          propertiesSet.add("NotNull");
        }
      }
    }
    return propertiesSet;
  }

  public final void setSubscribePackageName(final String packageName) {
    root.put(SUBSCRIBE_PACKAGE, packageName);
  }

  public final void setWrapperPackageName(final String packageName) {
    root.put(WRAPPER_PACKAGE, packageName);
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

  public final void addSchemaObject(final String modelPackage, final String keyClassName, final SchemaObject schemaObject, final Path filePath, final Path propertiesPath) {
    final var builder = ClassTemplate.builder().filePath(filePath).modelPackage(modelPackage).className(schemaObject.getClassName()).classSchema(schemaObject)
                                     .propertiesPath(propertiesPath);
    if (Objects.nonNull(keyClassName)) {
      builder.keyClassName(keyClassName);
    }
    schemaObjectMap.add(builder.build());
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

  public final void calculateJavaEEPackage(final Integer springBootVersion) {
    if (3 <= springBootVersion) {
      root.put("javaEEPackage", "jakarta");
    } else {
      root.put("javaEEPackage", "javax");
    }
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

  public final void fillTemplateWrapper(
      final Path filePath,
      final String modelPackage,
      final String classFullName,
      final String className,
      final String keyClassFullName,
      final String keyClassName
  ) throws TemplateException, IOException {
    final Map<String, Object> context = Map.of(WRAPPER_PACKAGE, modelPackage,
                                               "classNamespace", classFullName,
                                               "className", className,
                                               "keyNamespace", keyClassFullName,
                                               "keyClassName", keyClassName);

    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MESSAGE_WRAPPER, context, filePath.resolve("MessageWrapper.java").toAbsolutePath().toString());
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
                     checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_INTERFACE_SUPPLIERS), interfaceRoot);
      } else if (Objects.equals(method.getType(), "subscribe")) {
        fillTemplate(subscribeFilePath, "I" + method.getOperationId().substring(0, 1).toUpperCase() + method.getOperationId().substring(1),
                     checkTemplate(method.getBindingType(), TemplateIndexConstants.TEMPLATE_INTERFACE_CONSUMERS), interfaceRoot);
      }
    }
  }

  private void writeTemplateToFile(final String templateName, final Map<String, Object> root, final String path) throws IOException, TemplateException {
    final Template template = cfg.getTemplate(templateName);

    final FileWriter writer = new FileWriter(path);
    template.process(root, writer);
    writer.close();
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
