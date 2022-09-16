package net.coru.api.generator.plugin.model

import org.gradle.api.Action
import org.gradle.util.internal.ConfigureUtil

class OpenApiModelExtension {

  List<OpenApiSpecFile> openApiSpecFiles = new ArrayList<>()

  Boolean overWriteModel = Boolean.FALSE

  List<OpenApiSpecFile> getOpenApiSpecFiles() {
    return openApiSpecFiles
  }

  Boolean getOverWriteModel() {
    return overWriteModel
  }

  void openApiSpecFile(Closure configuration) {
    OpenApiSpecFile apiSpecFile = new OpenApiSpecFile()
    ConfigureUtil.configure(configuration, apiSpecFile)
    openApiSpecFiles.add(apiSpecFile)
  }

  void openApiSpecFile(Action<? super OpenApiSpecFile> configuration) {
    OpenApiSpecFile apiSpecFile = new OpenApiSpecFile()
    configuration.execute(apiSpecFile)
    openApiSpecFiles.add(apiSpecFile)
  }
}
