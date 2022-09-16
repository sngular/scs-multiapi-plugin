package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.model.AsyncApiModelExtension
import net.coru.api.generator.plugin.model.OpenApiModelExtension
import org.gradle.api.Plugin
import org.gradle.api.Project

class ScsMultiApi implements Plugin<Project> {

  @Override
  void apply(final Project project) {
    project.extensions.create("openapimodel", OpenApiModelExtension)
    project.extensions.create("asyncapimodel", AsyncApiModelExtension)
    project.task("openApiTask", type: OpenApiMultiApiTask.class)
    project.task("asyncApiTask", type: AsyncApiMultiApiTask.class)
  }
}
