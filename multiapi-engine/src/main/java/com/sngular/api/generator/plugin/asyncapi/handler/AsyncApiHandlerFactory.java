package com.sngular.api.generator.plugin.asyncapi.handler;

import java.io.File;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class AsyncApiHandlerFactory {

  private AsyncApiHandlerFactory() {
  }

  public static BaseAsyncApiHandler getHandler(
      final String apiVersion, final Integer springBootVersion,
      boolean overwriteModel,
      final File targetFolder,
      final String processedGeneratedSourcesFolder,
      final String groupId,
      final File baseDir) {
    BaseAsyncApiHandler handler;
    if (apiVersion.startsWith("3.")) {
      handler = new AsyncApi3Handler(springBootVersion, overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, baseDir);
    } else {
      handler = new AsyncApi2Handler(springBootVersion, overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, baseDir);
    }
    return handler;
  }
}