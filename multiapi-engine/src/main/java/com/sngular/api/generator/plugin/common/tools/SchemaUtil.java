package com.sngular.api.generator.plugin.common.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.openapi.exception.FileParseException;
import org.apache.commons.lang3.StringUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.Map;
import java.util.Objects;

public class SchemaUtil {

  static final ObjectMapper PARSER = new ObjectMapper(new YAMLFactory());

  protected SchemaUtil() {
  }

  public static JsonNode solveRef(final String refValue, final Map<String, JsonNode> schemaMap, final URI rootFilePath) {
    JsonNode solvedRef;
    if (StringUtils.isNotEmpty(refValue)) {
      if (refValue.startsWith("#")) {
        final String refSchemaName = MapperUtil.getRefSchemaKey(refValue);
        solvedRef = schemaMap.get(refSchemaName);
      } else {
        final var refValueArr = refValue.split("#");
        final var filePath = refValueArr[0];
        solvedRef = getPojoFromRef(rootFilePath, filePath);
        if (ApiTool.hasComponents(solvedRef)) {
          schemaMap.putAll(ApiTool.getComponentSchemas(solvedRef));
          solvedRef = solvedRef.findValue(MapperUtil.getKey(refValueArr[1]));
        }
      }
    } else {
      solvedRef = null;
    }
    return solvedRef;
  }

  public static JsonNode getPojoFromRef(final URI rootFilePath, final String refPath) {
    final JsonNode schemaFile;
    try {
      schemaFile = PARSER.readTree(readFile(rootFilePath, refPath));
    } catch (final IOException e) {
      throw new FileParseException(refPath, e);
    }

    if (Objects.isNull(schemaFile)) {
      throw new FileParseException("empty .yml");
    }

    return schemaFile;
  }


  private static String readFile(final URI rootFilePath, final String filePath) throws MalformedURLException {
    if (Objects.isNull(filePath)) {
      throw new IllegalArgumentException("File Path cannot be empty");
    }
    URL fileURL = SchemaUtil.class.getClassLoader().getResource(filePath);
    if (Objects.isNull(fileURL)) {
      final var parentFolder = rootFilePath.resolve(cleanUpPath(filePath));
      fileURL = parentFolder.toURL();
    }
    final var sb = new StringBuilder();
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(fileURL.openStream()))) {
      String inputLine;
      while ((inputLine = reader.readLine()) != null) {
        sb.append(inputLine).append(System.lineSeparator());
      }
    } catch (final IOException e) {
      throw new FileParseException("Error reading api file", e);
    }
    return sb.toString();
  }

  private static String cleanUpPath(final String filePath) {
    return StringUtils.startsWith(filePath, "./") ? filePath.substring(2) : filePath;
  }

}
