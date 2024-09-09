package com.sngular.api.generator.plugin.common.template;

import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.template.ClassTemplate;
import com.sngular.api.generator.plugin.asyncapi.template.ClasspathTemplateLoader;
import com.sngular.api.generator.plugin.asyncapi.template.TemplateIndexConstants;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.exception.GeneratorTemplateException;
import com.sngular.api.generator.plugin.openapi.exception.OverwritingApiFilesException;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public abstract class CommonTemplateFactory {

  private final static List<String> BASIC_DATA_TYPES = List.of("Integer", "Long", "Float", "Double", "Boolean", "String", "Char", "Byte", "Short");

  private static final String EXCEPTION_PACKAGE = "exceptionPackage";

  private static final String SLASH = "/";

  private static final String PACKAGE_SEPARATOR_STR = ".";

  private static final String FILE_TYPE_JAVA = ".java";

  private final Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);

  private final Map<String, Object> root = new HashMap<>();

  private final List<ClassTemplate> classTemplateList = new LinkedList<>();

  private final boolean checkOverwrite;

  private File targetFolder;

  private String processedGeneratedSourcesFolder;

  private final FilenameFilter targetFileFilter;

  private final File baseDir;

  public CommonTemplateFactory(boolean checkOverwrite,
                               final File targetFolder,
                               final String processedGeneratedSourcesFolder,
                               final File baseDir) {
    this.checkOverwrite = checkOverwrite;
    cfg.setTemplateLoader(new ClasspathTemplateLoader());
    cfg.setDefaultEncoding("UTF-8");
    cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
    cfg.setLogTemplateExceptions(true);
    cfg.setAPIBuiltinEnabled(true);
    addToRoot("checkBasicTypes", BASIC_DATA_TYPES);
    this.targetFolder = targetFolder;
    this.targetFileFilter =  (dir, name) -> name.toLowerCase().contains(targetFolder.toPath().getFileName().toString());
    this.processedGeneratedSourcesFolder = processedGeneratedSourcesFolder;
    this.baseDir = baseDir;
  }

  protected void generateTemplates(boolean generateExceptionTemplate) {

    final String exceptionPackage;
    if (Boolean.TRUE.equals(generateExceptionTemplate)) {
      exceptionPackage = getClassTemplate().getModelPackage();
    } else {
      exceptionPackage = null;
    }

    classTemplateList.forEach(classTemplate -> {
      try {
        fillTemplates(classTemplate.getPropertiesPath(), classTemplate.getModelPackage(),
            fillTemplateSchema(classTemplate, false, exceptionPackage));
      } catch (final IOException | TemplateException exception) {
        throw new FileSystemException(exception);
      }
    });
  }

  private ClassTemplate getClassTemplate() {
    ClassTemplate ourClassTemplate = null;
    for (ClassTemplate classTemplate : classTemplateList) {
      if (classTemplate.getFilePath().endsWith("schemas")) {
        ourClassTemplate = classTemplate;
        break;
      }
    }
    if (ourClassTemplate == null) {
      ourClassTemplate = classTemplateList.get(0);
    }

    return ourClassTemplate;
  }

  protected void fillTemplate(final String filePathToSave, final String className, final String templateName) throws IOException, TemplateException {
    final File fileToSave = Paths.get(filePathToSave).normalize().toFile();
    fileToSave.mkdirs();
    final String pathToSaveMainClass = fileToSave.toPath().resolve(className + FILE_TYPE_JAVA).toString();
    fillTemplate(pathToSaveMainClass, templateName);
  }

  private void fillTemplate(final String pathToSaveMainClass, final String templateName) throws IOException, TemplateException {
    writeTemplateToFile(templateName, pathToSaveMainClass);
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  protected void fillTemplates(final Path filePathToSave, final String modelPackage, final Set<String> fieldProperties) throws TemplateException, IOException {
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
          fillTemplateCustom(filePathToSave, modelPackage, "MaxInteger.java", TemplateIndexConstants.TEMPLATE_MAX_INTEGER_ANNOTATION,
              "MaxIntegerValidator.java", TemplateIndexConstants.TEMPLATE_MAX_INTEGER_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxBigDecimal.java", TemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_ANNOTATION,
              "MaxBigDecimalValidator.java", TemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxDouble.java", TemplateIndexConstants.TEMPLATE_MAX_DOUBLE_ANNOTATION,
              "MaxDoubleValidator.java", TemplateIndexConstants.TEMPLATE_MAX_DOUBLE_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxFloat.java", TemplateIndexConstants.TEMPLATE_MAX_FLOAT_ANNOTATION,
              "MaxFloatValidator.java", TemplateIndexConstants.TEMPLATE_MAX_FLOAT_VALIDATOR_ANNOTATION);
          break;
        case "Minimum":
          fillTemplateCustom(filePathToSave, modelPackage, "MinInteger.java", TemplateIndexConstants.TEMPLATE_MIN_INTEGER_ANNOTATION,
              "MinIntegerValidator.java", TemplateIndexConstants.TEMPLATE_MIN_INTEGER_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinDouble.java", TemplateIndexConstants.TEMPLATE_MIN_DOUBLE_ANNOTATION,
              "MinDoubleValidator.java", TemplateIndexConstants.TEMPLATE_MIN_DOUBLE_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinFloat.java", TemplateIndexConstants.TEMPLATE_MIN_FLOAT_ANNOTATION,
              "MinFloatValidator.java", TemplateIndexConstants.TEMPLATE_MIN_FLOAT_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinBigDecimal.java", TemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_ANNOTATION,
              "MinBigDecimalValidator.java", TemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_VALIDATOR_ANNOTATION);
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

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private Set<String> fillTemplateSchema(final ClassTemplate classTemplate, final Boolean useLombok, final String exceptionPackage)
      throws IOException, TemplateException {
    final var propertiesSet = new HashSet<String>();
    final var schemaObject = classTemplate.getClassSchema();
    final var filePath = classTemplate.getFilePath();
    if (Objects.nonNull(schemaObject) && Objects.nonNull(schemaObject.getFieldObjectList()) && !schemaObject.getFieldObjectList().isEmpty()) {
      addToRoot("schema", schemaObject);
      final String templateName = null != useLombok && useLombok ? TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK : TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA;
      if (Objects.nonNull(classTemplate.getModelPackage())) {
        addToRoot("packageModel", classTemplate.getModelPackage());
      }
      if (Objects.nonNull(exceptionPackage)) {
        addToRoot(EXCEPTION_PACKAGE, exceptionPackage);
      }
      fillTemplate(filePath.toString(), schemaObject.getClassName(), templateName);
      for (SchemaFieldObject fieldObject : schemaObject.getFieldObjectList()) {
        propertiesSet.addAll(fieldObject.getRestrictions().getProperties());
        if (fieldObject.isRequired() && Boolean.FALSE.equals(useLombok)) {
          propertiesSet.add("NotNull");
        }
      }
    }
    return propertiesSet;
  }

  public final void fillTemplateModelClassException(final Path filePathToSave, final String modelPackage) throws IOException {
    final Path pathToExceptionPackage = filePathToSave.resolve("exception");
    pathToExceptionPackage.toFile().mkdirs();
    addToRoot(EXCEPTION_PACKAGE, modelPackage);
    final String pathToSaveMainClass = pathToExceptionPackage.resolve("ModelClassException.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MODEL_EXCEPTION, pathToSaveMainClass);
  }

  private void fillTemplateCustom(
      final Path filePathToSave, final String modelPackage, final String fileNameAnnotation, final String templateAnnotation,
      final String fileNameValidator, final String templateValidator) throws TemplateException, IOException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    pathToCustomValidatorPackage.toFile().mkdirs();
    root.put("packageModel", modelPackage);
    final String pathToSaveAnnotationClass = pathToCustomValidatorPackage.resolve(fileNameAnnotation).toString();
    writeTemplateToFile(templateAnnotation, pathToSaveAnnotationClass);
    final String pathToSaveValidatorClass = pathToCustomValidatorPackage.resolve(fileNameValidator).toString();
    writeTemplateToFile(templateValidator, pathToSaveValidatorClass);
  }

  protected void addToRoot(final String key, final Object value) {
    root.put(key, value);
  }

  protected void addToRoot(final Map<String, Object> propertiesSet) {
    root.putAll(propertiesSet);
  }

  protected Object getFromRoot(final String key) {
    return root.get(key);
  }

  protected void cleanData() {
    root.clear();
    classTemplateList.clear();
  }

  public final void addSchemaObject(final String modelPackage,
                                    final String keyClassName,
                                    final SchemaObject schemaObject,
                                    final String destinationPackage) {
    final var filePath = processPath(getPath(StringUtils.defaultIfEmpty(destinationPackage, DEFAULT_ASYNCAPI_API_PACKAGE + SLASH + schemaObject.getParentPackage())));
    final var propertiesPath = processPath(getPath(modelPackage));
    final var builder = ClassTemplate.builder().filePath(filePath).modelPackage(modelPackage).className(schemaObject.getClassName()).classSchema(schemaObject)
        .propertiesPath(propertiesPath);
    if (Objects.nonNull(keyClassName)) {
      builder.keyClassName(keyClassName);
    }
    classTemplateList.add(builder.build());
  }

  protected void writeTemplateToFile(final String templateName, final String path) throws IOException {
    final Template template = cfg.getTemplate(templateName);

    if (!Files.exists(Path.of(path)) || checkOverwrite) {
      try (FileWriter writer = new FileWriter(path)) {
        template.process(root, writer);
      } catch (IOException | TemplateException exception) {
        final var schema = root.get("schema");
        throw new GeneratorTemplateException(String.format(" Error processing template %s with object %s", templateName, ((SchemaObject) schema).getClassName()), exception);
      }
    } else {
      throw new OverwritingApiFilesException();
    }
  }

  public Path processPath(final String packagePath) {
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

  protected String convertPackageToTargetPath(final OperationParameterObject operationParameter, final String defaultApiPackage) {
    String path = null;
    if (Objects.nonNull(operationParameter)) {
      if (Objects.nonNull(operationParameter.getApiPackage())) {
        path = getPath(operationParameter.getApiPackage());
      } else {
        path = getPath(defaultApiPackage);
      }
    }
    return path;
  }

  protected String getPath(final String pathName) {
    return processedGeneratedSourcesFolder + pathName.replace(PACKAGE_SEPARATOR_STR, SLASH);
  }
}
