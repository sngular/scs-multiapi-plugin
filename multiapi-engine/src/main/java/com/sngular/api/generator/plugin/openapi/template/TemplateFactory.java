/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.template;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Objects;

import com.sngular.api.generator.plugin.common.template.CommonTemplateFactory;
import com.sngular.api.generator.plugin.openapi.model.AuthObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.lang3.StringUtils;

public class TemplateFactory extends CommonTemplateFactory {

  private static final String DEFAULT_API_PACKAGE = "com.sngular.api";

  public TemplateFactory(
      boolean enableOverwrite,
      final File targetFolder,
      final String processedGeneratedSourcesFolder,
      final File baseDir) {
    super(enableOverwrite, targetFolder, processedGeneratedSourcesFolder, baseDir, new ClasspathTemplateLoader());
  }

  public final void clearData() {
    cleanData();
  }

  @Override
  protected void clearRoot() {
    delFromRoot("className");
    delFromRoot("pathObjects");
    delFromRoot("packageApi");
    delFromRoot("packageModel");
    delFromRoot("exceptionPackage");
    delFromRoot("authObject");
    delFromRoot("clientPackage");
    delFromRoot("javaEEPackage");
  }

  public final void fillTemplates() {
    generateTemplates();
  }

  public final void fillTemplateWebClient(final String filePathToSave) throws IOException {
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_WEB_CLIENT, filePathToSave, "ApiWebClient");
  }

  public final void fillTemplateRestClient(final String filePathToSave) throws IOException {
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_REST_CLIENT, filePathToSave, "ApiRestClient");
  }

  public final void fillTemplateAuth(final String apiPackage, final String authName) throws IOException {
    writeTemplateToFile(createNameTemplate(authName), apiPackage, authName);
  }

  private String createNameTemplate(final String classNameAuth) {
    return "template" + classNameAuth + ".ftlh";
  }

  public final void fillTemplate(
      final SpecFile specFile, final String className,
      final List<PathObject> pathObjects, final AuthObject authObject) throws IOException {

    addToRoot("className", className);
    addToRoot("pathObjects", pathObjects);

    if (Objects.nonNull(specFile.getApiPackage())) {
      addToRoot("packageApi", StringUtils.defaultIfEmpty(specFile.getApiPackage(), DEFAULT_API_PACKAGE));
    }
    if (Objects.nonNull(specFile.getModelPackage())) {
      addToRoot("packageModel", specFile.getModelPackage());
      addToRoot("exceptionPackage", specFile.getModelPackage());
    }

    if (specFile.isCallMode()) {
      addToRoot("authObject", authObject);
      addToRoot("clientPackage", specFile.getClientPackage());
      addToRoot("clientComponent", specFile.shouldRegisterClientComponent());
      addToRoot("isReactive", specFile.isReactive());
    }

    writeTemplateToFile(specFile.isCallMode() ? getTemplateClientApi(specFile) : getTemplateApi(specFile),
                        StringUtils.defaultIfEmpty(specFile.getApiPackage(), DEFAULT_API_PACKAGE), className + "Api");
  }

  private String getTemplateClientApi(final SpecFile specFile) {
    final String template;
    if (specFile.isUseHttpExchange()) {
      template = TemplateIndexConstants.TEMPLATE_CALL_HTTP_EXCHANGE_API;
    } else {
      template = specFile.isReactive() ? TemplateIndexConstants.TEMPLATE_CALL_WEB_API : TemplateIndexConstants.TEMPLATE_CALL_REST_API;
    }
    return template;
  }

  private String getTemplateApi(final SpecFile specFile) {
    return specFile.isReactive() ? TemplateIndexConstants.TEMPLATE_REACTIVE_API : TemplateIndexConstants.TEMPLATE_INTERFACE_API;
  }

  public final void calculateJavaEEPackage(final Integer springBootVersion) {
    if (3 <= springBootVersion) {
      addToRoot("javaEEPackage", "jakarta");
    } else {
      addToRoot("javaEEPackage", "javax");
    }
  }

  public final void calculateJacksonPackage(final Integer springBootVersion) {
    // Spring Boot 4 (Spring Framework 7) ships Jackson 3, which relocated its databind/core
    // packages from com.fasterxml.jackson to tools.jackson. Jackson annotations keep the old
    // coordinates, so only the databind package is switched here.
    if (4 <= springBootVersion) {
      addToRoot("jacksonPackage", "tools.jackson");
      addToRoot("isJackson3", Boolean.TRUE);
    } else {
      addToRoot("jacksonPackage", "com.fasterxml.jackson");
      addToRoot("isJackson3", Boolean.FALSE);
    }
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

}
