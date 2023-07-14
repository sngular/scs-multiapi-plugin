/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin

import com.sngular.api.generator.plugin.model.AsyncApiModelExtension
import com.sngular.api.generator.plugin.model.OpenApiModelExtension
import org.gradle.api.Plugin
import org.gradle.api.Project

class ScsMultiApi implements Plugin<Project> {

  @Override
  void apply(final Project project) {
    project.plugins.apply("java")
    project.extensions.create("openapimodel", OpenApiModelExtension)
    project.extensions.create("asyncapimodel", AsyncApiModelExtension)
    def outputDirDefault = new File(project.buildDir.absolutePath + '/generated-source')
    outputDirDefault.mkdirs()
    def openApiTask = project.getTasks().register("openApiTask", OpenApiTask.class, {
      it.outputDir = outputDirDefault
    })
    def asyncApiTask = project.getTasks().register("asyncApiTask", AsyncApiTask.class, {
      it.outputDir = outputDirDefault
    })
    project.tasks.named('compileJava').configure {
      it.dependsOn(openApiTask)
      it.dependsOn(asyncApiTask)
    }
  }
}
