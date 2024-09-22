package com.sngular.api.generator.plugin.common.template;

import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.template.ClassTemplate;
import com.sngular.api.generator.plugin.asyncapi.template.ClasspathTemplateLoader;
import com.sngular.api.generator.plugin.asyncapi.template.TemplateIndexConstants;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
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

  private static final List<String> BASIC_DATA_TYPES = List.of("Integer", "Long", "Float", "Double", "Boolean", "String", "Char", "Byte", "Short");

  private static final String EXCEPTION_PACKAGE = "exceptionPackage";

  private static final String SLASH = "/";

  private static final String PACKAGE_SEPARATOR_STR = ".";

  private static final String FILE_TYPE_JAVA = ".java";

  private final Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);

  private final Map<String, Object> root = new HashMap<>();

  private final List<ClassTemplate> classTemplateList = new LinkedList<>();

  private final boolean checkOverwrite;

  private final File targetFolder;

  private final String processedGeneratedSourcesFolder;

  private final FilenameFilter targetFileFilter;

  private final File baseDir;

  protected boolean generateExceptionTemplate;

  protected CommonTemplateFactory(boolean checkOverwrite,
                               final File targetFolder,
                               final String processedGeneratedSourcesFolder,
                               final File baseDir,
                               final CommonTemplateLoader classpathTemplateLoader) {
    this.checkOverwrite = checkOverwrite;
    cfg.setTemplateLoader(classpathTemplateLoader);
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

  protected void generateTemplates() {

    final String exceptionPackage;
    if (Boolean.TRUE.equals(generateExceptionTemplate)) {
      exceptionPackage = getClassTemplate().getModelPackage();
    } else {
      exceptionPackage = null;
    }

    classTemplateList.forEach(classTemplate -> {
      try {
        fillTemplates(classTemplate.getPropertiesPath(), classTemplate.getModelPackage(),
            fillTemplateSchema(classTemplate, exceptionPackage));
        if (generateExceptionTemplate) {
          fillTemplateModelClassException(classTemplate.getModelPackage());
        }
      } catch (final IOException | TemplateException exception) {
        throw new FileSystemException(exception);
      }
    });
  }

  private ClassTemplate getClassTemplate() {
    ClassTemplate ourClassTemplate = null;
    final var classTemplateListIt = classTemplateList.iterator();
    while (Objects.isNull(ourClassTemplate) && classTemplateListIt.hasNext()) {
      final var classTemplate = classTemplateListIt.next();
      if (classTemplate.getFilePath().endsWith("schemas")) {
        ourClassTemplate = classTemplate;
      }
    }
    if (ourClassTemplate == null) {
      ourClassTemplate = classTemplateList.get(0);
    }

    return ourClassTemplate;
  }

  protected void fillTemplate(final String filePathToSave, final String className, final String templateName) throws IOException, TemplateException {
    final var fileToSave = Paths.get(filePathToSave);
    fileToSave.toFile().mkdirs();
    writeTemplateToFile(templateName, fileToSave, className);
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  protected void fillTemplates(final Path filePathToSave, final String modelPackage, final Set<String> fieldProperties) throws TemplateException, IOException {
    for (final String current : fieldProperties) {
      switch (current) {
        case "Size":
          fillTemplateCustom(filePathToSave, modelPackage, "Size", TemplateIndexConstants.TEMPLATE_SIZE_ANNOTATION, "SizeValidator",
              TemplateIndexConstants.TEMPLATE_SIZE_VALIDATOR_ANNOTATION);
          break;
        case "Pattern":
          fillTemplateCustom(filePathToSave, modelPackage, "Pattern", TemplateIndexConstants.TEMPLATE_PATTERN_ANNOTATION,
              "PatternValidator", TemplateIndexConstants.TEMPLATE_PATTERN_VALIDATOR_ANNOTATION);
          break;
        case "MultipleOf":
          fillTemplateCustom(filePathToSave, modelPackage, "MultipleOf", TemplateIndexConstants.TEMPLATE_MULTIPLEOF_ANNOTATION,
              "MultipleOfValidator", TemplateIndexConstants.TEMPLATE_MULTIPLEOF_VALIDATOR_ANNOTATION);
          break;
        case "Maximum":
          fillTemplateCustom(filePathToSave, modelPackage, "MaxInteger", TemplateIndexConstants.TEMPLATE_MAX_INTEGER_ANNOTATION,
              "MaxIntegerValidator", TemplateIndexConstants.TEMPLATE_MAX_INTEGER_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxBigDecimal", TemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_ANNOTATION,
              "MaxBigDecimalValidator", TemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxDouble", TemplateIndexConstants.TEMPLATE_MAX_DOUBLE_ANNOTATION,
              "MaxDoubleValidator", TemplateIndexConstants.TEMPLATE_MAX_DOUBLE_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxFloat", TemplateIndexConstants.TEMPLATE_MAX_FLOAT_ANNOTATION,
              "MaxFloatValidator", TemplateIndexConstants.TEMPLATE_MAX_FLOAT_VALIDATOR_ANNOTATION);
          break;
        case "Minimum":
          fillTemplateCustom(filePathToSave, modelPackage, "MinInteger", TemplateIndexConstants.TEMPLATE_MIN_INTEGER_ANNOTATION,
              "MinIntegerValidator", TemplateIndexConstants.TEMPLATE_MIN_INTEGER_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinDouble", TemplateIndexConstants.TEMPLATE_MIN_DOUBLE_ANNOTATION,
              "MinDoubleValidator", TemplateIndexConstants.TEMPLATE_MIN_DOUBLE_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinFloat", TemplateIndexConstants.TEMPLATE_MIN_FLOAT_ANNOTATION,
              "MinFloatValidator", TemplateIndexConstants.TEMPLATE_MIN_FLOAT_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinBigDecimal", TemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_ANNOTATION,
              "MinBigDecimalValidator", TemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_VALIDATOR_ANNOTATION);
          break;
        case "MaxItems":
          fillTemplateCustom(filePathToSave, modelPackage, "MaxItems", TemplateIndexConstants.TEMPLATE_MAX_ITEMS_ANNOTATION,
              "MaxItemsValidator", TemplateIndexConstants.TEMPLATE_MAX_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "MinItems":
          fillTemplateCustom(filePathToSave, modelPackage, "MinItems", TemplateIndexConstants.TEMPLATE_MIN_ITEMS_ANNOTATION,
              "MinItemsValidator", TemplateIndexConstants.TEMPLATE_MIN_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "NotNull":
          fillTemplateCustom(filePathToSave, modelPackage, "NotNull", TemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION,
              "NotNullValidator", TemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION);
          break;
        case "UniqueItems":
          fillTemplateCustom(filePathToSave, modelPackage, "UniqueItems", TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_ANNOTATION,
              "UniqueItemsValidator", TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_VALIDATOR_ANNOTATION);
          break;
        default:
          break;
      }
    }
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private Set<String> fillTemplateSchema(final ClassTemplate classTemplate, final String exceptionPackage)
      throws IOException, TemplateException {
    final var propertiesSet = new HashSet<String>();
    final var schemaObject = classTemplate.getClassSchema();
    final var filePath = classTemplate.getFilePath();
    if (Objects.nonNull(schemaObject) && Objects.nonNull(schemaObject.getFieldObjectList()) && !schemaObject.getFieldObjectList().isEmpty()) {
      addToRoot("schema", schemaObject);
      final String templateName = classTemplate.isUseLombok() ? TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK : TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA;
      if (Objects.nonNull(classTemplate.getModelPackage())) {
        addToRoot("packageModel", classTemplate.getModelPackage());
      }
      if (Objects.nonNull(exceptionPackage)) {
        addToRoot(EXCEPTION_PACKAGE, exceptionPackage);
      }
      fillTemplate(filePath.toString(), schemaObject.getClassName(), templateName);
      for (SchemaFieldObject fieldObject : schemaObject.getFieldObjectList()) {
        propertiesSet.addAll(fieldObject.getRestrictions().getProperties());
        if (fieldObject.isRequired() && Boolean.FALSE.equals(classTemplate.isUseLombok())) {
          propertiesSet.add("NotNull");
        }
      }
    }
    return propertiesSet;
  }

  public final void fillTemplateModelClassException(final String modelPackage) throws IOException {
    addToRoot(EXCEPTION_PACKAGE, modelPackage);
    writeTemplateToFile(CommonTemplateIndexConstants.TEMPLATE_MODEL_EXCEPTION, MapperUtil.packageToFolder(modelPackage) + SLASH + "exception", "ModelClassException");
  }

  public void setNotGenerateTemplate() {
    this.generateExceptionTemplate = false;
  }

  private void fillTemplateCustom(
      final Path filePathToSave, final String modelPackage, final String fileNameAnnotation, final String templateAnnotation,
      final String fileNameValidator, final String templateValidator) throws IOException {
    final Path pathToCustomValidatorPackage = filePathToSave.resolve("customvalidator");
    if (!pathToCustomValidatorPackage.toFile().exists()) {
      if (!pathToCustomValidatorPackage.toFile().mkdirs()) {
        throw new RuntimeException("Can't create custom validator directory");
      }
    }
    root.put("packageModel", modelPackage);
    writeTemplateToFile(templateAnnotation, pathToCustomValidatorPackage, fileNameAnnotation);
    writeTemplateToFile(templateValidator, pathToCustomValidatorPackage, fileNameValidator);
  }

  protected void addToRoot(final String key, final Object value) {
    root.put(key, value);
  }

  protected void addToRoot(final Map<String, Object> propertiesSet) {
    root.putAll(propertiesSet);
  }

  protected void cleanData() {
    root.clear();
    addToRoot("checkBasicTypes", BASIC_DATA_TYPES);
    classTemplateList.clear();
  }

  public final void addSchemaObject(final String modelPackage,
                                    final String keyClassName,
                                    final SchemaObject schemaObject,
                                    final String destinationPackage,
                                    final boolean useLombok) {
    final var filePath = processPath(getPath(destinationPackage));
    final var propertiesPath = processPath(getPath(modelPackage));
    final var builder = ClassTemplate
            .builder()
            .filePath(filePath)
            .modelPackage(modelPackage)
            .className(schemaObject.getClassName())
            .classSchema(schemaObject)
            .propertiesPath(propertiesPath)
        .useLombok(useLombok);
    if (Objects.nonNull(keyClassName)) {
      builder.keyClassName(keyClassName);
    }
    classTemplateList.add(builder.build());
  }

  protected void writeTemplateToFile(final String templateName, final String apiPackage, final String partialPath) throws IOException {
    writeTemplateToFile(templateName, processPath(getPath(apiPackage)), partialPath);
  }

  protected void writeTemplateToFile(final String templateName, final Path filePathToSave, final String partialPath) throws IOException {
    if (!filePathToSave.toFile().exists()) {
      if (!filePathToSave.toFile().mkdirs()) {
        throw new IOException("Could not create directory: " + filePathToSave.toFile().getAbsolutePath());
      }
    }
    final String path = filePathToSave.resolve(partialPath + FILE_TYPE_JAVA).toString();
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
    return processedGeneratedSourcesFolder + SLASH + pathName.replace(PACKAGE_SEPARATOR_STR, SLASH);
  }

  public void checkRequiredOrCombinatorExists(final SchemaObject schema, final boolean useLombok) {
    if ("anyOf".equals(schema.getSchemaCombinator()) || "oneOf".equals(schema.getSchemaCombinator())) {
      generateExceptionTemplate = true;
    } else if (Objects.nonNull(schema.getFieldObjectList()) && !useLombok) {
      final var fieldListIt = schema.getFieldObjectList().iterator();
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
}
