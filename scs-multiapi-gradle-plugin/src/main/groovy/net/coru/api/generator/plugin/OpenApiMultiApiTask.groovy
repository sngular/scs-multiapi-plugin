package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.model.OpenApiModelExtension
import net.coru.api.generator.plugin.openapi.OpenApiGenerator
import org.gradle.api.DefaultTask
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.tasks.OutputFile
import org.gradle.api.tasks.TaskAction

abstract class OpenApiMultiApiTask extends DefaultTask {

  @OutputFile
  abstract RegularFileProperty getTargetFolder()

  @TaskAction
  def processApiFile() {
    def targetFolder = getTargetFolder().get().asFile
    OpenApiModelExtension openApiExtension = project.openapimodel
    if (null != openApiExtension && !openApiExtension.openapiFiles.isEmpty()) {
      project.getBuildDir().absolutePath
      def generatedSourcesFolder = project.getBuildDir().absolutePath + "/" + PluginConstants.GENERATED_SOURCES_API_GENERATOR_FOLDER
      def asyncApiGen = new OpenApiGenerator(openApiExtension.overWriteModel, generatedSourcesFolder, project.getGroup() as String, targetFolder, project.getProjectDir())
      asyncApiGen.processFileSpec(openApiExtension.openapiFiles)
    }
  }
}
