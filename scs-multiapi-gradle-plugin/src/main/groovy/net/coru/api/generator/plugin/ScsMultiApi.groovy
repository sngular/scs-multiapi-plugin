package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.model.AsyncApiModelExtension
import net.coru.api.generator.plugin.model.OpenApiModelExtension
import org.gradle.api.Plugin
import org.gradle.api.Project

class ScsMultiApi implements Plugin<Project> {

  @Override
  void apply(final Project project) {
    project.getPluginManager().apply("java")
    project.extensions.create("openapimodel", OpenApiModelExtension)
    project.extensions.create("asyncapimodel", AsyncApiModelExtension)
    def openApiTask = project.task("openApiTask", type: OpenApiMultiApiTask.class)
    def outputDirDefault = new File(project.getBuildDir().getAbsolutePath() + '/generated-source')
    outputDirDefault.mkdirs()
    openApiTask.configure {
      it.outputDir = outputDirDefault
    }
    def asyncApiTask = project.task("asyncApiTask", type: AsyncApiMultiApiTask.class)
    asyncApiTask.configure {
      it.outputDir = outputDirDefault
    }
  }
}
