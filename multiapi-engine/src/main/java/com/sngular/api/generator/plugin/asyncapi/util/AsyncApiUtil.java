package com.sngular.api.generator.plugin.asyncapi.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.common.tools.SchemaUtil;

import java.nio.file.Path;

public final class AsyncApiUtil {

    private AsyncApiUtil() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    public static JsonNode getPojoFromSpecFile(final Path baseDir, final SpecFile specFile) {

        return SchemaUtil.getPojoFromRef(baseDir.toUri(), specFile.getFilePath());
    }
}
