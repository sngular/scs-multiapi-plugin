package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.model.OpenApiModelExtension
import net.coru.api.generator.plugin.model.OpenApiSpecFile
import net.coru.api.generator.plugin.openapi.OpenApiGenerator
import net.coru.api.generator.plugin.openapi.parameter.FileSpec
import org.gradle.api.DefaultTask
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction

abstract class OpenApiMultiApiTask extends DefaultTask {

  @OutputDirectory
  abstract DirectoryProperty getOutputDir()

  @TaskAction
  def processOpenApApiFile() {
    def targetFolder = getOutputDir().getAsFile().get()
    OpenApiModelExtension openApiExtension = getProject().getExtensions().getByType(OpenApiModelExtension.class)
    if (null != openApiExtension && !openApiExtension.getOpenApiSpecFiles().isEmpty()) {
      def generatedSourcesFolder = targetFolder.absolutePath + "/" + PluginConstants.GENERATED_SOURCES_API_GENERATOR_FOLDER
      def overWriteFlag = openApiExtension.getOverWriteModel().orElse(false).get()
      def openApiGen = new OpenApiGenerator(overWriteFlag, generatedSourcesFolder, project.getGroup() as String, targetFolder, project.getProjectDir())
      List<FileSpec> openApiSpecFiles = []
      openApiExtension.getOpenApiSpecFiles().forEach(apiSpec -> {
        openApiSpecFiles.add(toFileSpec(apiSpec))
      })
      openApiGen.processFileSpec(openApiSpecFiles)
    }
  }

  static def toFileSpec(OpenApiSpecFile openApiSpecFile) {
    def builder = FileSpec.builder()
    if (openApiSpecFile.filePath.isPresent()) {
      builder.filePath(openApiSpecFile.filePath.get())
    }
    if (openApiSpecFile.apiPackage.isPresent()) {
      builder.apiPackage(openApiSpecFile.apiPackage.get())
    }
    if (openApiSpecFile.modelPackage.isPresent()) {
      builder.modelPackage(openApiSpecFile.modelPackage.get())
    }
    if (openApiSpecFile.modelNamePrefix.isPresent()) {
      builder.modelNamePrefix(openApiSpecFile.modelNamePrefix.get())
    }
    if (openApiSpecFile.modelNameSuffix.isPresent()) {
      builder.modelNameSuffix(openApiSpecFile.modelNameSuffix.get())
    }
    if (openApiSpecFile.clientPackage.isPresent()) {
      builder.clientPackage(openApiSpecFile.clientPackage.get())
    }
    if (openApiSpecFile.callMode.isPresent()) {
      builder.callMode(openApiSpecFile.callMode.get())
    }
    if (openApiSpecFile.useTagsGroup.isPresent()) {
      builder.useTagsGroup(openApiSpecFile.useTagsGroup.get())
    }
    if (openApiSpecFile.useLombokModelAnnotation.isPresent()) {
      builder.useLombokModelAnnotation(openApiSpecFile.useLombokModelAnnotation.get())
    }
    if (openApiSpecFile.isReactive.isPresent()) {
      builder.isReactive(openApiSpecFile.isReactive.get())
    }

    return builder.build()
  }
}
