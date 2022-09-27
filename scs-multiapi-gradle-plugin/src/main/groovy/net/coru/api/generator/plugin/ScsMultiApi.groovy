/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.model.AsyncApiModelExtension
import net.coru.api.generator.plugin.model.OpenApiModelExtension
import org.gradle.api.Plugin
import org.gradle.api.Project

class ScsMultiApi implements Plugin<Project> {

  @Override
  void apply(final Project project) {
    project.getPlugins().apply("java")
    project.extensions.create("openapimodel", OpenApiModelExtension)
    project.extensions.create("asyncapimodel", AsyncApiModelExtension)
    def openApiTask = project.task("openApiTask", type: OpenApiTask.class)
    def outputDirDefault = new File(project.getBuildDir().getAbsolutePath() + '/generated-source')
    outputDirDefault.mkdirs()
    openApiTask.configure {
      it.outputDir = outputDirDefault
    }
    def asyncApiTask = project.task("asyncApiTask", type: AsyncApiTask.class)
    asyncApiTask.configure {
      it.outputDir = outputDirDefault
    }
  }
}
