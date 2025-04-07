package com.sngular.api.generator.plugin.common.template;

import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.template.ClassTemplate;
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

  private static String getTemplateName(ClassTemplate classTemplate) {
    String templateName;
    if (classTemplate.getClassSchema().isEnum()) {
      templateName = CommonTemplateIndexConstants.TEMPLATE_CONTENT_ENUM;
    } else {
      templateName = classTemplate.isUseLombok() ? CommonTemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK : CommonTemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA;
    }
    return templateName;
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
      } catch (final IOException exception) {
        throw new FileSystemException(exception);
      }
    });
  }

  protected void fillTemplate(final String filePathToSave, final String className, final String templateName) throws IOException {
    final var fileToSave = Paths.get(filePathToSave);
    fileToSave.toFile().mkdirs();
    writeTemplateToFile(templateName, fileToSave, className);
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  protected void fillTemplates(final Path filePathToSave, final String modelPackage, final Set<String> fieldProperties) throws IOException {
    for (final String current : fieldProperties) {
      switch (current) {
        case "Size":
          fillTemplateCustom(filePathToSave, modelPackage, "Size", CommonTemplateIndexConstants.TEMPLATE_SIZE_ANNOTATION, "SizeValidator",
            CommonTemplateIndexConstants.TEMPLATE_SIZE_VALIDATOR_ANNOTATION);
          break;
        case "Pattern":
          fillTemplateCustom(filePathToSave, modelPackage, "Pattern", CommonTemplateIndexConstants.TEMPLATE_PATTERN_ANNOTATION,
            "PatternValidator", CommonTemplateIndexConstants.TEMPLATE_PATTERN_VALIDATOR_ANNOTATION);
          break;
        case "MultipleOf":
          fillTemplateCustom(filePathToSave, modelPackage, "MultipleOf", CommonTemplateIndexConstants.TEMPLATE_MULTIPLEOF_ANNOTATION,
            "MultipleOfValidator", CommonTemplateIndexConstants.TEMPLATE_MULTIPLEOF_VALIDATOR_ANNOTATION);
          break;
        case "Maximum":
          fillTemplateCustom(filePathToSave, modelPackage, "MaxInteger", CommonTemplateIndexConstants.TEMPLATE_MAX_INTEGER_ANNOTATION,
            "MaxIntegerValidator", CommonTemplateIndexConstants.TEMPLATE_MAX_INTEGER_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxBigDecimal", CommonTemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_ANNOTATION,
            "MaxBigDecimalValidator", CommonTemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxDouble", CommonTemplateIndexConstants.TEMPLATE_MAX_DOUBLE_ANNOTATION,
            "MaxDoubleValidator", CommonTemplateIndexConstants.TEMPLATE_MAX_DOUBLE_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MaxFloat", CommonTemplateIndexConstants.TEMPLATE_MAX_FLOAT_ANNOTATION,
            "MaxFloatValidator", CommonTemplateIndexConstants.TEMPLATE_MAX_FLOAT_VALIDATOR_ANNOTATION);
          break;
        case "Minimum":
          fillTemplateCustom(filePathToSave, modelPackage, "MinInteger", CommonTemplateIndexConstants.TEMPLATE_MIN_INTEGER_ANNOTATION,
            "MinIntegerValidator", CommonTemplateIndexConstants.TEMPLATE_MIN_INTEGER_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinDouble", CommonTemplateIndexConstants.TEMPLATE_MIN_DOUBLE_ANNOTATION,
            "MinDoubleValidator", CommonTemplateIndexConstants.TEMPLATE_MIN_DOUBLE_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinFloat", CommonTemplateIndexConstants.TEMPLATE_MIN_FLOAT_ANNOTATION,
            "MinFloatValidator", CommonTemplateIndexConstants.TEMPLATE_MIN_FLOAT_VALIDATOR_ANNOTATION);
          fillTemplateCustom(filePathToSave, modelPackage, "MinBigDecimal", CommonTemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_ANNOTATION,
            "MinBigDecimalValidator", CommonTemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_VALIDATOR_ANNOTATION);
          break;
        case "MaxItems":
          fillTemplateCustom(filePathToSave, modelPackage, "MaxItems", CommonTemplateIndexConstants.TEMPLATE_MAX_ITEMS_ANNOTATION,
            "MaxItemsValidator", CommonTemplateIndexConstants.TEMPLATE_MAX_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "MinItems":
          fillTemplateCustom(filePathToSave, modelPackage, "MinItems", CommonTemplateIndexConstants.TEMPLATE_MIN_ITEMS_ANNOTATION,
            "MinItemsValidator", CommonTemplateIndexConstants.TEMPLATE_MIN_ITEMS_VALIDATOR_ANNOTATION);
          break;
        case "NotNull":
          fillTemplateCustom(filePathToSave, modelPackage, "NotNull", CommonTemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION,
            "NotNullValidator", CommonTemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION);
          break;
        case "UniqueItems":
          fillTemplateCustom(filePathToSave, modelPackage, "UniqueItems", CommonTemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_ANNOTATION,
            "UniqueItemsValidator", CommonTemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_VALIDATOR_ANNOTATION);
          break;
        default:
          break;
      }
    }
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private Set<String> fillTemplateSchema(final ClassTemplate classTemplate, final String exceptionPackage)
    throws IOException {
    final var propertiesSet = new HashSet<String>();
    final var schemaObject = classTemplate.getClassSchema();
    final var filePath = classTemplate.getFilePath();
    if (Objects.nonNull(schemaObject) && Objects.nonNull(schemaObject.getFieldObjectList()) && !schemaObject.getFieldObjectList().isEmpty()) {
      addToRoot("schema", schemaObject);
      final String templateName = getTemplateName(classTemplate);
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
    if (!pathToCustomValidatorPackage.toFile().exists() && !pathToCustomValidatorPackage.toFile().mkdirs()) {
      throw new IOException("Can't create custom validator directory");
    }
    root.put("packageModel", modelPackage);
    writeTemplateToFile(templateAnnotation, pathToCustomValidatorPackage, fileNameAnnotation);
    writeTemplateToFile(templateValidator, pathToCustomValidatorPackage, fileNameValidator);
  }

  protected void addToRoot(final String key, final Object value) {
    root.put(key, value);
  }

  protected void delFromRoot(final String key) {
    root.remove(key);
  }

  protected void addToRoot(final Map<String, Object> propertiesSet) {
    root.putAll(propertiesSet);
  }

  protected void cleanData() {
    clearRoot();
    addToRoot("checkBasicTypes", BASIC_DATA_TYPES);
    classTemplateList.clear();
    generateExceptionTemplate = false;
  }

  protected abstract void clearRoot();

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
    if (!filePathToSave.toFile().exists() && !filePathToSave.toFile().mkdirs()) {
      throw new IOException("Could not create directory: " + filePathToSave.toFile().getAbsolutePath());
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
    return StringUtils.replace(path, "//", SLASH);
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
