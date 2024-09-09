/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.template;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.sngular.api.generator.plugin.common.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.template.CommonTemplateFactory;
import com.sngular.api.generator.plugin.exception.GeneratorTemplateException;
import com.sngular.api.generator.plugin.openapi.exception.OverwritingApiFilesException;
import com.sngular.api.generator.plugin.openapi.model.AuthObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;

public class TemplateFactory extends CommonTemplateFactory {

  public TemplateFactory(boolean enableOverwrite) {
    super(enableOverwrite);
  }

  public final void fillTemplateSchema(
      final String filePathToSave, final Boolean useLombok, final SchemaObject schemaObject,
      final Set<String> propertiesSet) throws IOException,
                                            TemplateException {
    final File fileToSave = new File(filePathToSave);
    if (Objects.nonNull(schemaObject.getFieldObjectList()) && !schemaObject.getFieldObjectList().isEmpty()) {
      addToRoot("schema", schemaObject);
      final String pathToSaveMainClass = fileToSave.toPath().resolve(schemaObject.getClassName() + JAVA_EXTENSION).toString();
      writeTemplateToFile(templateSelector(useLombok, schemaObject), pathToSaveMainClass);
      for (SchemaFieldObject fieldObject : schemaObject.getFieldObjectList()) {
        propertiesSet.addAll(fieldObject.getRestrictions().getProperties());
        if (fieldObject.isRequired() && Boolean.FALSE.equals(useLombok)) {
          propertiesSet.add("NotNull");
        }
      }
    }
  }

  public final void clearData() {
    cleanData();
  }

  private static String templateSelector(final Boolean useLombok, final SchemaObject schemaObject) {
    final var shouldUseLombok = Objects.requireNonNullElse(useLombok, false);
    final String template;

    if (Boolean.TRUE.equals(shouldUseLombok)) {
      template = TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK;
    } else {
      template = TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA;
    }

    return template;
  }

  public final void fillTemplateModelClassException(final String filePathToSave, final boolean overwriteEnabled, String modelPackage) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToExceptionPackage = fileToSave.toPath().resolve("exception");
    pathToExceptionPackage.toFile().mkdirs();
    final String pathToSaveMainClass = pathToExceptionPackage.resolve("ModelClassException.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MODEL_EXCEPTION, root, pathToSaveMainClass, overwriteEnabled);

  }

  public final void fillTemplateWebClient(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);

    final String pathToSaveMainClass = fileToSave.toPath().resolve("ApiWebClient.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_WEB_CLIENT, pathToSaveMainClass);

  }

  public final void fillTemplateRestClient(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);

    final String pathToSaveMainClass = fileToSave.toPath().resolve("ApiRestClient.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_REST_CLIENT, pathToSaveMainClass);

  }

  public final void fillTemplateAuth(final String filePathToSave, final String authName) throws IOException {
    final File fileToSave = new File(filePathToSave);
    final var nameAuthClass = authName + JAVA_EXTENSION;
    final String pathToSaveMainClass = fileToSave.toPath().resolve(nameAuthClass).toString();
    writeTemplateToFile(createNameTemplate(authName), pathToSaveMainClass);

  }

  public final void fillTemplateCustom(
      final String filePathToSave, final String annotationFileName, final String validatorFileName, final String annotationTemplate,
      final String validatorTemplate) throws IOException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToValidatorPackage = fileToSave.toPath().resolve("customvalidator");
    pathToValidatorPackage.toFile().mkdirs();
    final String pathToSaveMainClass = pathToValidatorPackage.resolve(annotationFileName).toString();
    writeTemplateToFile(annotationTemplate, pathToSaveMainClass);
    final String pathToSaveMainClassValidator = pathToValidatorPackage.resolve(validatorFileName).toString();
    writeTemplateToFile(validatorTemplate, pathToSaveMainClassValidator);
  }

  public final void fillTemplate(
      final String filePathToSave, final SpecFile specFile, final String className,
      final List<PathObject> pathObjects, final AuthObject authObject) throws IOException, TemplateException {

    addToRoot("className", className);
    addToRoot("pathObjects", pathObjects);

    if (Objects.nonNull(specFile.getApiPackage())) {
      addToRoot("packageApi", specFile.getApiPackage());
    }
    if (Objects.nonNull(specFile.getModelPackage())) {
      addToRoot("packageModel", specFile.getModelPackage());
      addToRoot("exceptionPackage", specFile.getModelPackage());
    }
    final File fileToSave = new File(filePathToSave);

    if (specFile.isCallMode()) {
      addToRoot("authObject", authObject);
    }

    final String pathToSaveMainClass = fileToSave.toPath().resolve(className + "Api" + JAVA_EXTENSION).toString();
    writeTemplateToFile(specFile.isCallMode() ? getTemplateClientApi(specFile) : getTemplateApi(specFile), pathToSaveMainClass);

  }

  public final void calculateJavaEEPackage(final Integer springBootVersion) {
    if (3 <= springBootVersion) {
      addToRoot("javaEEPackage", "jakarta");
    } else {
      addToRoot("javaEEPackage", "javax");
    }
  }

  void writeTemplateToFile(final String templateName, final String path) throws IOException {
    writeTemplateToFile(templateName, path, true);
  }

  public final void setPackageName(final String packageName) {
    addToRoot("package", packageName);
  }

  public final void setModelPackageName(final String packageName) {
    addToRoot("packageModel", packageName);
  }

  public final void setWebClientPackageName(final String packageName) {
    addToRoot("packageClient", packageName);
  }

  public final void setAuthPackageName(final String packageName) {
    addToRoot("packageAuth", packageName);
  }

  private String createNameTemplate(final String classNameAuth) {
    return "template" + classNameAuth + ".ftlh";
  }

  private String getTemplateClientApi(final SpecFile specFile) {
    return specFile.isReactive() ? TemplateIndexConstants.TEMPLATE_CALL_WEB_API : TemplateIndexConstants.TEMPLATE_CALL_REST_API;
  }

  private String getTemplateApi(final SpecFile specFile) {
    return specFile.isReactive() ? TemplateIndexConstants.TEMPLATE_REACTIVE_API : TemplateIndexConstants.TEMPLATE_INTERFACE_API;
  }

}
