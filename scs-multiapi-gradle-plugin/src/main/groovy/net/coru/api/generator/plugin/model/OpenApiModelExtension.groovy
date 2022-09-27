package net.coru.api.generator.plugin.model

import org.gradle.api.Action
import org.gradle.util.internal.ConfigureUtil

class OpenApiModelExtension {

  List<OpenApiSpecFile> specFile = new ArrayList<>()

  Boolean overWriteModel = Boolean.FALSE

  List<OpenApiSpecFile> getSpecFile() {
    return specFile
  }

  Boolean getOverWriteModel() {
    return overWriteModel
  }

  void specFile(Closure configuration) {
    OpenApiSpecFile apiSpecFile = new OpenApiSpecFile()
    ConfigureUtil.configure(configuration, apiSpecFile)
    specFile.add(apiSpecFile)
  }

  void specFile(Action<? super OpenApiSpecFile> configuration) {
    OpenApiSpecFile apiSpecFile = new OpenApiSpecFile()
    configuration.execute(apiSpecFile)
    specFile.add(apiSpecFile)
  }
}
