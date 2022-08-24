package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.asyncapi.AsyncApiGenerator
import net.coru.api.generator.plugin.model.AsyncApiModel
import org.gradle.api.DefaultTask
import org.gradle.api.Project
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.provider.Property
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.OutputFile
import org.gradle.api.tasks.TaskAction

abstract class AsyncApiMultiApiTask extends DefaultTask {

  @Input
  abstract Property<AsyncApiModel> getAsyncApiConfiguration()

  @OutputFile
  abstract RegularFileProperty getTargetFolder()

  @TaskAction
  def processApiFile(Project project) {
    def targetFolder = getTargetFolder().get().asFile
    if (getAsyncApiConfiguration().isPresent()) {
      project.getBuildDir().absolutePath
      def generatedSourcesFolder = project.getBuildDir().absolutePath + "/" + PluginConstants.GENERATED_SOURCES_API_GENERATOR_FOLDER
      def asyncApiGen = new AsyncApiGenerator(targetFolder, generatedSourcesFolder, project.getGroup() as String, project.getProjectDir())
      asyncApiGen.processFileSpec(getAsyncApiConfiguration().get().fileSpecs)
    }
  }
}
