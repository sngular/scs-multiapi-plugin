package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.model.OpenApiModel
import net.coru.api.generator.plugin.openapi.OpenApiGenerator
import org.gradle.api.DefaultTask
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.provider.Property
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.OutputFile
import org.gradle.api.tasks.SkipWhenEmpty
import org.gradle.api.tasks.TaskAction

abstract class OpenApiMultiApiTask extends DefaultTask {

  @Input
  @SkipWhenEmpty
  abstract Property<OpenApiModel> openApiConfiguration

  @Input
  abstract Property<Boolean> overwriteModel

  @OutputFile
  abstract RegularFileProperty getTargetFolder()

  @TaskAction
  def processApiFile() {
    def targetFolder = getTargetFolder().get().asFile
    if (openApiConfiguration.isPresent()) {
      project.getBuildDir().absolutePath
      def generatedSourcesFolder = project.getBuildDir().absolutePath + "/" + PluginConstants.GENERATED_SOURCES_API_GENERATOR_FOLDER
      def asyncApiGen = new OpenApiGenerator(overwriteModel.orElse(Boolean.FALSE).get(), generatedSourcesFolder, project.getGroup() as String, targetFolder, project.getProjectDir())
      asyncApiGen.processFileSpec(openApiConfiguration.get().openapiFiles)
    }
  }
}
