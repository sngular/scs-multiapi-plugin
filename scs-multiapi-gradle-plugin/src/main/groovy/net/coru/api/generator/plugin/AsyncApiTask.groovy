/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
package net.coru.api.generator.plugin

import net.coru.api.generator.plugin.asyncapi.AsyncApiGenerator
import net.coru.api.generator.plugin.asyncapi.parameter.OperationParameterObject
import net.coru.api.generator.plugin.asyncapi.parameter.SpecFile
import net.coru.api.generator.plugin.model.AsyncApiModelExtension
import net.coru.api.generator.plugin.model.AsyncApiSpecFile
import net.coru.api.generator.plugin.model.OperationParameter
import org.gradle.api.DefaultTask
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.tasks.Optional
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction

abstract class AsyncApiTask extends DefaultTask {

  @Optional
  @OutputDirectory
  abstract DirectoryProperty getOutputDir()

  @TaskAction
  def processAsyncApiFile() {
    def targetFolder = getOrCreateTargetFolder(getOutputDir())
    def generatedDir = getOrCreateGenerated(getOutputDir())
    AsyncApiModelExtension asyncApiModelExtension = getProject().getExtensions().getByType(AsyncApiModelExtension.class)
    if (null != asyncApiModelExtension && !asyncApiModelExtension.getSpecFile().isEmpty()) {
      def asyncApiGen = new AsyncApiGenerator(targetFolder, generatedDir, project.getGroup() as String, project.getProjectDir())
      List<SpecFile> asyncApiSpecFiles = []
      asyncApiModelExtension.getSpecFile().forEach(apiSpec -> {
        asyncApiSpecFiles.add(toFileSpec(apiSpec))
      })

      asyncApiGen.processFileSpec(asyncApiSpecFiles)
    }
  }

  static File getOrCreateTargetFolder(DirectoryProperty outputDir) {
    def generated = new File("build/generated/")
    if (outputDir.isPresent()) {
      generated = outputDir.getAsFile().get()
    } else {
      generated.mkdirs()
    }
    return generated
  }

  static def getOrCreateGenerated(DirectoryProperty outputDir) {
    def generated = new File("generated/sources/annotationProcessor/main")
    if (outputDir.isPresent()) {
      generated = outputDir.getAsFile().get()
    } else {
      generated.mkdirs()
    }
    return generated.absolutePath + "/"
  }

  static def toFileSpec(AsyncApiSpecFile apiSpecFile) {
    def builder = SpecFile.builder()
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
    if (parameterObject.getApiPackage()) {
      builder.apiPackage(parameterObject.apiPackage)
    }

    if (parameterObject.getClassNamePostfix()) {
      builder.classNamePostfix(parameterObject.classNamePostfix)
    }
    if (parameterObject.getIds()) {
      builder.ids(parameterObject.ids)
    }
    if (parameterObject.getModelNameSuffix()) {
      builder.modelNameSuffix(parameterObject.modelNameSuffix)
    }
    if (parameterObject.getModelPackage()) {
      builder.modelPackage(parameterObject.modelPackage)
    }

    return builder.build()
  }
}
