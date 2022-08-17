/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.template;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import net.coru.api.generator.plugin.openapi.exception.OverwritingApiFilesException;
import net.coru.api.generator.plugin.openapi.model.AuthObject;
import net.coru.api.generator.plugin.openapi.model.PathObject;
import net.coru.api.generator.plugin.openapi.model.SchemaObject;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;

public class TemplateFactory {

  private final Configuration cfg = new Configuration(Configuration.VERSION_2_3_27);

  private final Map<String, Object> root = new HashMap<>();

  public TemplateFactory() {
    cfg.setTemplateLoader(new ClasspathTemplateLoader());
    cfg.setDefaultEncoding("UTF-8");
    cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
    cfg.setLogTemplateExceptions(true);
    final List<String> basicDataTypes = List.of("Integer", "Long", "Float", "Double", "Boolean", "String", "Char", "Byte", "Short");
    root.put("checkBasicTypes", basicDataTypes);

  }

  public final void fillTemplateSchema(final String filePathToSave, final Boolean useLombok, final SchemaObject schemaObject) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    if (Objects.nonNull(schemaObject.getFieldObjectList()) && !schemaObject.getFieldObjectList().isEmpty()) {
      root.put("schema", schemaObject);
      final String pathToSaveMainClass = fileToSave.toPath().resolve(schemaObject.getClassName() + ".java").toString();
      writeTemplateToFile(null != useLombok && useLombok ? TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK : TemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA, root,
                          pathToSaveMainClass);
    }

  }

  public final void fillTemplateModelClassException(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final Path pathToExceptionPackage = fileToSave.toPath().resolve("exception");
    pathToExceptionPackage.toFile().mkdirs();
    final String pathToSaveMainClass = pathToExceptionPackage.resolve("ModelClassException.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_MODEL_EXCEPTION, root, pathToSaveMainClass, false);

  }

  public final void fillTemplateWebClient(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);

    final String pathToSaveMainClass = fileToSave.toPath().resolve("ApiWebClient.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_WEB_CLIENT, root, pathToSaveMainClass);

  }

  public final void fillTemplateRestClient(final String filePathToSave) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);

    final String pathToSaveMainClass = fileToSave.toPath().resolve("ApiRestClient.java").toString();
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_REST_CLIENT, root, pathToSaveMainClass);

  }

  public final void fillTemplateAuth(final String filePathToSave, final String authName) throws IOException, TemplateException {
    final File fileToSave = new File(filePathToSave);
    final var nameAuthClass = authName + ".java";
    final String pathToSaveMainClass = fileToSave.toPath().resolve(nameAuthClass).toString();
    writeTemplateToFile(createNameTemplate(authName), root, pathToSaveMainClass);

  }

  public final void fillTemplate(
      final String filePathToSave, final FileSpec fileSpec, final String className,
      final List<PathObject> pathObjects, final AuthObject authObject) throws IOException, TemplateException {

    root.put("className", className);
    root.put("pathObjects", pathObjects);

    if (Objects.nonNull(fileSpec.getApiPackage())) {
      root.put("packageApi", fileSpec.getApiPackage());
    }
    if (Objects.nonNull(fileSpec.getModelPackage())) {
      root.put("packageModel", fileSpec.getModelPackage());
    }
    final File fileToSave = new File(filePathToSave);

    if (fileSpec.isCallMode()) {
      root.put("authObject", authObject);
    }

    final String pathToSaveMainClass = fileToSave.toPath().resolve(className + "Api" + ".java").toString();
    writeTemplateToFile(fileSpec.isCallMode() ? getTemplateClientApi(fileSpec) : getTemplateApi(fileSpec), root, pathToSaveMainClass);

  }

  private void writeTemplateToFile(final String templateName, final Map<String, Object> root, final String path) throws IOException, TemplateException {
    writeTemplateToFile(templateName, root, path, true);
  }

  private void writeTemplateToFile(final String templateName, final Map<String, Object> root, final String path, final boolean checkOverwrite) throws IOException,
                                                                                                                                                      TemplateException {
    final Template template = cfg.getTemplate(templateName);

    if (!Files.exists(Path.of(path)) || !checkOverwrite) {
      final FileWriter writer = new FileWriter(path);
      template.process(root, writer);
      writer.close();
    } else {
      throw new OverwritingApiFilesException();
    }
  }

  public final void setPackageName(final String packageName) {
    root.put("package", packageName);
  }

  public final void setModelPackageName(final String packageName) {
    root.put("packageModel", packageName);
  }

  public final void setWebClientPackageName(final String packageName) {
    root.put("packageClient", packageName);
  }

  public final void setAuthPackageName(final String packageName) {
    root.put("packageAuth", packageName);
  }

  private String createNameTemplate(final String classNameAuth) {
    return "template" + classNameAuth + ".ftlh";
  }

  private String getTemplateClientApi(final FileSpec fileSpec) {
    return fileSpec.isReactive() ? TemplateIndexConstants.TEMPLATE_CALL_WEB_API : TemplateIndexConstants.TEMPLATE_CALL_REST_API;
  }

  private String getTemplateApi(final FileSpec fileSpec) {
    return fileSpec.isReactive() ? TemplateIndexConstants.TEMPLATE_REACTIVE_API : TemplateIndexConstants.TEMPLATE_INTERFACE_API;
  }

}
