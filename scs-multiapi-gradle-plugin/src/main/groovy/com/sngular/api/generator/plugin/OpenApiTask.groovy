/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin


import com.sngular.api.generator.plugin.model.OpenApiModelExtension
import com.sngular.api.generator.plugin.model.OpenApiSpecFile
import org.gradle.api.DefaultTask
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction

abstract class OpenApiTask extends DefaultTask {

  @Optional
  @OutputDirectory
  abstract DirectoryProperty getOutputDir()

  @TaskAction
  def processOpenApApiFile() {
    def targetFolder = getOrCreateTargetFolder(getOutputDir())
    def generatedDir = getOrCreateGenerated(getOutputDir())
    OpenApiModelExtension openApiExtension = getProject().getExtensions().getByType(OpenApiModelExtension.class)
    if (null != openApiExtension && !openApiExtension.getSpecFile().isEmpty()) {
      def openApiGen = new com.sngular.api.generator.plugin.openapi.OpenApiGenerator(openApiExtension.getOverWriteModel(), generatedDir, project.getGroup() as String, targetFolder, project.getProjectDir())
      List<com.sngular.api.generator.plugin.openapi.parameter.SpecFile> openApiSpecFiles = []
      openApiExtension.getSpecFile().forEach(apiSpec -> {
        openApiSpecFiles.add(toFileSpec(apiSpec))
      })
      openApiGen.processFileSpec(openApiSpecFiles)
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
    return generated.absolutePath
  }

  static def toFileSpec(OpenApiSpecFile openApiSpecFile) {
    def builder = com.sngular.api.generator.plugin.openapi.parameter.SpecFile.builder()
    if (openApiSpecFile.filePath) {
      builder.filePath(openApiSpecFile.filePath)
    }
    if (openApiSpecFile.apiPackage) {
      builder.apiPackage(openApiSpecFile.apiPackage)
    }
    if (openApiSpecFile.modelPackage) {
      builder.modelPackage(openApiSpecFile.modelPackage)
    }
    if (openApiSpecFile.modelNamePrefix) {
      builder.modelNamePrefix(openApiSpecFile.modelNamePrefix)
    }
    if (openApiSpecFile.modelNameSuffix) {
      builder.modelNameSuffix(openApiSpecFile.modelNameSuffix)
    }
    if (openApiSpecFile.clientPackage) {
      builder.clientPackage(openApiSpecFile.clientPackage)
    }
    if (openApiSpecFile.callMode) {
      builder.callMode(openApiSpecFile.callMode)
    }
    if (openApiSpecFile.useTagsGroup) {
      builder.useTagsGroup(openApiSpecFile.useTagsGroup)
    }
    if (openApiSpecFile.useLombokModelAnnotation) {
      builder.useLombokModelAnnotation(openApiSpecFile.useLombokModelAnnotation)
    }
    if (openApiSpecFile.isReactive) {
      builder.isReactive(openApiSpecFile.isReactive)
    }

    return builder.build()
  }
}
