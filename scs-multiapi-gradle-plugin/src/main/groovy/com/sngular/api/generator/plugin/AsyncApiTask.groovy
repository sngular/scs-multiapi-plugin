/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin

import com.sngular.api.generator.plugin.asyncapi.AsyncApiGenerator
import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile
import com.sngular.api.generator.plugin.model.AsyncApiModelExtension
import com.sngular.api.generator.plugin.model.AsyncApiSpecFile
import com.sngular.api.generator.plugin.model.OperationParameter
import org.gradle.api.DefaultTask
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.tasks.Optional
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction
import org.gradle.work.DisableCachingByDefault

@DisableCachingByDefault(because = "Generation depends on external spec files not declared as cacheable inputs")
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
      def asyncApiGen = new AsyncApiGenerator(asyncApiModelExtension.getSpringBootVersion(), asyncApiModelExtension.getOverWriteModel(), targetFolder, generatedDir, project.getGroup() as String, project.getProjectDir())
      List<SpecFile> asyncApiSpecFiles = []
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
    builder.generateModelOnly(Boolean.TRUE.equals(apiSpecFile.getGenerateModelOnly()))

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
    if (parameterObject.getModelNamePrefix()) {
      builder.modelNamePrefix(parameterObject.modelNamePrefix)
    }
    if (parameterObject.getModelNameSuffix()) {
      builder.modelNameSuffix(parameterObject.modelNameSuffix)
    }
    if (parameterObject.getModelPackage()) {
      builder.modelPackage(parameterObject.modelPackage)
    }
    if (parameterObject.getDateFormat()) {
      builder.dateFormat(parameterObject.dateFormat)
    }
    if (parameterObject.getDateTimeFormat()) {
      builder.dateTimeFormat(parameterObject.dateTimeFormat)
    }
    if (parameterObject.getUseTimeType()) {
      builder.useTimeType(parameterObject.useTimeType)
    }
    if (parameterObject.getUseLombokModelAnnotation()) {
      builder.useLombokModelAnnotation(parameterObject.useLombokModelAnnotation)
    }
    if (parameterObject.getGenerateSpringwolfAnnotations()) {
      builder.generateSpringwolfAnnotations(parameterObject.generateSpringwolfAnnotations)
    }
    if (parameterObject.getUsePactAnnotation()) {
      builder.usePactAnnotation(parameterObject.usePactAnnotation)
    }

    return builder.build()
  }
}
