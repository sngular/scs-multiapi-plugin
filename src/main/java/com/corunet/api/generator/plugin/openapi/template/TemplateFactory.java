/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.openapi.template;

import static com.corunet.api.generator.plugin.openapi.template.TemplateIndex.TEMPLATE_CALL_REST_API;
import static com.corunet.api.generator.plugin.openapi.template.TemplateIndex.TEMPLATE_CALL_WEB_API;
import static com.corunet.api.generator.plugin.openapi.template.TemplateIndex.TEMPLATE_CONTENT_SCHEMA;
import static com.corunet.api.generator.plugin.openapi.template.TemplateIndex.TEMPLATE_CONTENT_SCHEMA_LOMBOK;
import static com.corunet.api.generator.plugin.openapi.template.TemplateIndex.TEMPLATE_INTERFACE_API;
import static com.corunet.api.generator.plugin.openapi.template.TemplateIndex.TEMPLATE_REACTIVE_API;
import static com.corunet.api.generator.plugin.openapi.template.TemplateIndex.TEMPLATE_REST_CLIENT;
import static com.corunet.api.generator.plugin.openapi.template.TemplateIndex.TEMPLATE_WEB_CLIENT;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.corunet.api.generator.plugin.openapi.parameter.FileSpec;
import com.corunet.api.generator.plugin.openapi.model.AuthObject;
import com.corunet.api.generator.plugin.openapi.model.PathObject;
import com.corunet.api.generator.plugin.openapi.model.SchemaObject;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;

public class TemplateFactory {

  private final Configuration cfg = new Configuration(Configuration.VERSION_2_3_27);

  private final Map<String, Object> root = new HashMap<>();

  private final HashMap<String, PathItem> itemHashMap = new HashMap<>();

  private final HashMap<String, Schema> itemSchema = new HashMap<>();


  public TemplateFactory() {
    cfg.setTemplateLoader(new ClasspathTemplateLoader());
    cfg.setDefaultEncoding("UTF-8");
    cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
    cfg.setLogTemplateExceptions(true);

  }

  public void fillTemplateSchema(String filePathToSave, Boolean useLombock, SchemaObject schemaObject) throws IOException, TemplateException {
    root.put("schema", schemaObject);
    root.put("stringBracketOpen", "{");
    root.put("stringBracketClose", "}");
    File fileToSave = new File(filePathToSave);

    String pathToSaveMainClass = fileToSave.toPath().resolve(schemaObject.getClassName() + ".java").toString();
    writeTemplateToFile(null != useLombock && useLombock ? TEMPLATE_CONTENT_SCHEMA_LOMBOK : TEMPLATE_CONTENT_SCHEMA, root, pathToSaveMainClass);

  }

  public void fillTemplateWebClient(String filePathToSave) throws IOException, TemplateException {
    File fileToSave = new File(filePathToSave);

    String pathToSaveMainClass = fileToSave.toPath().resolve("ApiWebClient.java").toString();
    writeTemplateToFile(TEMPLATE_WEB_CLIENT, root, pathToSaveMainClass);

  }

  public void fillTemplateRestClient(String filePathToSave) throws IOException, TemplateException {
    File fileToSave = new File(filePathToSave);

    String pathToSaveMainClass = fileToSave.toPath().resolve("ApiRestClient.java").toString();
    writeTemplateToFile(TEMPLATE_REST_CLIENT, root, pathToSaveMainClass);

  }

  public void fillTemplateAuth(String filePathToSave, String authName) throws IOException, TemplateException {
    File fileToSave = new File(filePathToSave);
    var nameAuthClass = authName + ".java";
    String pathToSaveMainClass = fileToSave.toPath().resolve(nameAuthClass).toString();
    writeTemplateToFile(createNameTemplate(authName), root, pathToSaveMainClass);

  }

  public void fillTemplate(String filePathToSave, FileSpec fileSpec, String className,
      ArrayList<PathObject> pathObject, AuthObject authObject) throws IOException, TemplateException {

    root.put("className", className);
    root.put("itemHashMap", itemHashMap);
    root.put("pathObject", pathObject);

    if (Objects.nonNull(fileSpec.getApiPackage())) {
      root.put("packageApi", fileSpec.getApiPackage());
    }
    if (Objects.nonNull(fileSpec.getModelPackage())) {
      root.put("packageModel", fileSpec.getModelPackage());
    }
    File fileToSave = new File(filePathToSave);

    if(fileSpec.getCallMode()) {
      root.put("authObject", authObject);
    }

    String pathToSaveMainClass = fileToSave.toPath().resolve(className + "Api" + ".java").toString();
    writeTemplateToFile(fileSpec.getCallMode() ? getTemplateClientApi(fileSpec) : getTemplateApi(fileSpec), root, pathToSaveMainClass);

  }


  private void writeTemplateToFile(String templateName, Map<String, Object> root, String path) throws IOException, TemplateException {
    Template template = cfg.getTemplate(templateName);

    FileWriter writer = new FileWriter(path);
    template.process(root, writer);
    writer.close();
  }

  public void setPackageName(String packageName) {
    root.put("package", packageName);
  }

  public void setModelPackageName(String packageName) {
    root.put("packageModel", packageName);
  }

  public void setWebClientPackageName(String packageName) {
    root.put("packageClient", packageName);
  }

  public void setAuthPackageName(String packageName) {
    root.put("packageAuth", packageName);
  }

  public void addPathItems(HashMap<String, PathItem> itemMap) {
    itemHashMap.putAll(itemMap);
  }

  public void addComponents(Map<String, Schema> itemMap) {
    itemSchema.putAll(itemMap);
  }

  private String createNameTemplate(String classNameAuth){
    var buffer = new StringBuffer(0);
    buffer.append("template").append(classNameAuth).append(".ftlh");
    return buffer.toString();
  }

  private String getTemplateClientApi(FileSpec fileSpec){
    return fileSpec.getIsReactive() ? TEMPLATE_CALL_WEB_API : TEMPLATE_CALL_REST_API;
  }

  private String getTemplateApi(FileSpec fileSpec){
    return fileSpec.getIsReactive() ? TEMPLATE_REACTIVE_API : TEMPLATE_INTERFACE_API;
  }

}
