/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.template;

import com.sngular.api.generator.plugin.common.template.CommonTemplateFactory;
import com.sngular.api.generator.plugin.openapi.model.AuthObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import freemarker.template.TemplateException;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Objects;

public class TemplateFactory extends CommonTemplateFactory {

  private static final String DEFAULT_API_PACKAGE = "com.sngular.api";

  public TemplateFactory(boolean enableOverwrite,
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

  public final void fillTemplateWebClient(final String filePathToSave) throws IOException, TemplateException {
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_WEB_CLIENT, filePathToSave, "ApiWebClient");
  }

  public final void fillTemplateRestClient(final String filePathToSave) throws IOException, TemplateException {
    writeTemplateToFile(TemplateIndexConstants.TEMPLATE_REST_CLIENT, filePathToSave, "ApiRestClient");
  }

  public final void fillTemplateAuth(final String apiPackage, final String authName) throws IOException {
    writeTemplateToFile(createNameTemplate(authName), apiPackage, authName);
  }

  public final void fillTemplate(final SpecFile specFile, final String className,
      final List<PathObject> pathObjects, final AuthObject authObject) throws IOException, TemplateException {

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
    }

    writeTemplateToFile(specFile.isCallMode() ? getTemplateClientApi(specFile) : getTemplateApi(specFile),
      StringUtils.defaultIfEmpty(specFile.getApiPackage(), DEFAULT_API_PACKAGE), className + "Api");
  }

  public final void calculateJavaEEPackage(final Integer springBootVersion) {
    if (3 <= springBootVersion) {
      addToRoot("javaEEPackage", "jakarta");
    } else {
      addToRoot("javaEEPackage", "javax");
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
