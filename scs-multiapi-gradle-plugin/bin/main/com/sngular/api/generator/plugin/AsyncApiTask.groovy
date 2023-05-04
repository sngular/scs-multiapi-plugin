/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin


import com.sngular.api.generator.plugin.model.AsyncApiModelExtension
import com.sngular.api.generator.plugin.model.AsyncApiSpecFile
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
    if (null != asyncApiModelExtension && !asyncApiModelExtension.getSpecFiles().isEmpty()) {
      def asyncApiGen = new com.sngular.api.generator.plugin.asyncapi.AsyncApiGenerator(targetFolder, generatedDir, project.getGroup() as String, project.getProjectDir())
      List<com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile> asyncApiSpecFiles = []
      asyncApiModelExtension.getSpecFiles().forEach(apiSpec -> {
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
    def builder = com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile.builder()
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

  static com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject toOperationParameterObject(com.sngular.api.generator.plugin.model.OperationParameter parameterObject) {
    def builder = com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject.builder()
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
