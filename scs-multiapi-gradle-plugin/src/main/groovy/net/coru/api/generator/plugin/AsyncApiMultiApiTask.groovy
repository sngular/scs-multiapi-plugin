package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.asyncapi.AsyncApiGenerator
import net.coru.api.generator.plugin.asyncapi.parameter.FileSpec
import net.coru.api.generator.plugin.asyncapi.parameter.OperationParameterObject
import net.coru.api.generator.plugin.model.AsyncApiModelExtension
import net.coru.api.generator.plugin.model.AsyncApiSpecFile
import net.coru.api.generator.plugin.model.OperationParameter
import org.gradle.api.DefaultTask
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction

abstract class AsyncApiMultiApiTask extends DefaultTask {

  @OutputDirectory
  abstract DirectoryProperty getOutputDir()

  @TaskAction
  def processAsyncApiFile() {
    def targetFolder = getOutputDir().getAsFile().get()
    AsyncApiModelExtension asyncApiModelExtension = getProject().getExtensions().getByType(AsyncApiModelExtension.class)
    if (null != asyncApiModelExtension && !asyncApiModelExtension.getAsyncApiSpecFiles().isEmpty()) {
      def generatedSourcesFolder = targetFolder.absolutePath + "/" + PluginConstants.GENERATED_SOURCES_API_GENERATOR_FOLDER
      def asyncApiGen = new AsyncApiGenerator(targetFolder, generatedSourcesFolder, project.getGroup() as String, project.getProjectDir())
      List<FileSpec> asyncApiSpecFiles = []
      asyncApiModelExtension.getAsyncApiSpecFiles().forEach(apiSpec -> {
        asyncApiSpecFiles.add(toFileSpec(apiSpec))
      })

      asyncApiGen.processFileSpec(asyncApiSpecFiles)
    }
  }

  static def toFileSpec(AsyncApiSpecFile apiSpecFile) {
    def builder = FileSpec.builder()
    if (!apiSpecFile.filePath.isEmpty()) {
      builder.filePath(apiSpecFile.getFilePath())
    }
    if (apiSpecFile.consumer) {
      builder.consumer(toOperationParameterObject(apiSpecFile.consumer))
    }
    if (apiSpecFile.streamBridge) {
      builder.streamBridge(toOperationParameterObject(apiSpecFile.streamBridge))
    }
    if (apiSpecFile.supplier) {
      builder.supplier(toOperationParameterObject(apiSpecFile.supplier))
    }

    return builder.build()
  }

  static OperationParameterObject toOperationParameterObject(OperationParameter parameterObject) {
    def builder = OperationParameterObject.builder()
    if (!parameterObject.getApiPackage()) {
      builder.apiPackage(parameterObject.apiPackage)
    }

    if (!parameterObject.getClassNamePostfix()) {
      builder.classNamePostfix(parameterObject.classNamePostfix)
    }
    if (!parameterObject.getIds()) {
      builder.ids(parameterObject.ids)
    }
    if (!parameterObject.getModelNameSuffix()) {
      builder.modelNameSuffix(parameterObject.modelNameSuffix)
    }
    if (!parameterObject.getModelPackage()) {
      builder.modelPackage(parameterObject.modelPackage)
    }
    if (!parameterObject.getOperationIds()) {
      builder.operationIds(parameterObject.operationIds)
    }

    return builder.build()
  }
}
