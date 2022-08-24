package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.model.AsyncApiModelExtension
import net.coru.api.generator.plugin.model.OpenApiModelExtension
import org.gradle.api.Plugin
import org.gradle.api.Project

class ScsMultiApi implements Plugin<Project> {

  @Override
  void apply(final Project project) {
    project.extensions.add("openapimodel", OpenApiModelExtension)
    project.extensions.add("asyncapimodel", AsyncApiModelExtension)
    project.task("OpenApiTask", type: OpenApiMultiApiTask.class)
    project.task("AsyncApiTask", type: AsyncApiMultiApiTask.class)
  }
}
