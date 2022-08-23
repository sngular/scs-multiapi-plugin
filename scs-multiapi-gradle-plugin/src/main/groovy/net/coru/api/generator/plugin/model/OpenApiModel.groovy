package net.coru.api.generator.plugin.model

import net.coru.api.generator.plugin.openapi.parameter.FileSpec

class OpenApiModel {

  List<FileSpec> openapiFiles

  Boolean overwriteModel

  String generatedSourcesFolder

  String processedGeneratedSourcesFolder

  File targetFolder
}
