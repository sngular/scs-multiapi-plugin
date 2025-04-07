package com.sngular.api.generator.plugin.asyncapi.handler;

import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public final class AsyncApiHandlerFactory {
    private static AsyncApiHandlerFactory instance;
    private final Map<String, BaseAsyncApiHandler> handlers;

    private AsyncApiHandlerFactory() {
        handlers = new HashMap<>();
    }

    public static AsyncApiHandlerFactory getInstance() {
        if (instance == null) {
            instance = new AsyncApiHandlerFactory();
        }
        return instance;
    }

    public BaseAsyncApiHandler getHandler(final String apiVersion, final Integer springBootVersion,
                                          boolean overwriteModel,
                                          final File targetFolder,
                                          final String processedGeneratedSourcesFolder,
                                          final String groupId,
                                          final File baseDir) {
        BaseAsyncApiHandler handler;
        if (apiVersion.startsWith("3.")) {
            handler = handlers.computeIfAbsent(apiVersion, version -> {
                log.debug("Creating AsyncAPI V3 handler for Spring Boot version: {}", version);
                return new AsyncApi3Handler(springBootVersion, overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, baseDir);
            });
        } else {
            handler = handlers.computeIfAbsent(apiVersion, version -> {
                log.debug("Creating AsyncAPI handler for Spring Boot version: {}", version);
                return new AsyncApi2Handler(springBootVersion, overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, baseDir);
            });
        }
        return handler;
    }
}