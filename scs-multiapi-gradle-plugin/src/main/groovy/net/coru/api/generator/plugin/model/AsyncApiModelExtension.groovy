package net.coru.api.generator.plugin.model

import org.gradle.api.Action
import org.gradle.util.internal.ConfigureUtil

class AsyncApiModelExtension {

  List<AsyncApiSpecFile> asyncApiSpecFiles = new ArrayList<>()

  Boolean overWriteModel = Boolean.FALSE

  List<AsyncApiSpecFile> getAsyncApiSpecFiles() {
    return asyncApiSpecFiles
  }

  Boolean getOverWriteModel() {
    return overWriteModel
  }

  void asyncApiSpecFile(Closure configuration) {
    AsyncApiSpecFile apiSpecFile = new AsyncApiSpecFile()
    ConfigureUtil.configure(configuration, apiSpecFile)
    asyncApiSpecFiles.add(apiSpecFile)
  }

  void asyncApiSpecFile(Action<? super AsyncApiSpecFile> configuration) {
    AsyncApiSpecFile apiSpecFile = new AsyncApiSpecFile()
    configuration.execute(apiSpecFile)
    asyncApiSpecFiles.add(apiSpecFile)
  }
}
