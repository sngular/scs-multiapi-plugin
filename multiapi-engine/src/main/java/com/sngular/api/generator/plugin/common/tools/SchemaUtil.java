package com.sngular.api.generator.plugin.common.tools;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.openapi.exception.FileParseException;
import org.apache.commons.lang3.StringUtils;

public class SchemaUtil {

  static final ObjectMapper PARSER = new ObjectMapper(new YAMLFactory());

  protected SchemaUtil() {
  }

  public static JsonNode solveRef(final String refValue, final Map<String, JsonNode> schemaMap, final URI rootFilePath) {
    JsonNode solvedRef;
    if (StringUtils.isNotEmpty(refValue)) {
      if (refValue.startsWith("#")) {
        final String refSchemaName = MapperUtil.getRefSchemaKey(refValue);
        // try direct
        solvedRef = schemaMap.get(refSchemaName);
        // if not found, try PascalCase variant
        if (Objects.isNull(solvedRef)) {
          final String pascal = toPascalCase(refSchemaName);
          if (!Objects.equals(pascal, refSchemaName)) {
            solvedRef = schemaMap.get(pascal);
          }
        }
      } else {
        final var refValueArr = refValue.split("#");
        final var filePath = refValueArr[0];
        final JsonNode fileNode = getPojoFromRef(rootFilePath, filePath);
        // If the referenced file contains components, insert them into schemaMap (original and PascalCase keys)
        if (ApiTool.hasComponents(fileNode)) {
          final Map<String, JsonNode> components = ApiTool.getComponentSchemas(fileNode);
          for (final Map.Entry<String, JsonNode> entry : components.entrySet()) {
            final String originalKey = entry.getKey();
            final JsonNode value = entry.getValue();
            final String normalizedKey = toPascalCase(originalKey);
            schemaMap.put(originalKey, value);
            if (!Objects.equals(originalKey, normalizedKey)) {
              schemaMap.put(normalizedKey, value);
            }
          }
        }
        // If a fragment is provided after '#', attempt to extract that node from the file; otherwise return the whole file
        if (refValueArr.length > 1 && StringUtils.isNotEmpty(refValueArr[1])) {
          final String fragment = refValueArr[1];
          final String key = MapperUtil.getKey(fragment);
          solvedRef = fileNode.findValue(key);
        } else {
          // No fragment provided: return the root document. The caller can decide how to register it.
          solvedRef = fileNode;
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

    // Normalize the incoming filePath: remove leading './' and replace backslashes with forward slashes
    final String cleaned = cleanUpPath(filePath).replace('\\', '/');

    // First, try to find the file in the classpath using the cleaned path
    URL fileURL = SchemaUtil.class.getClassLoader().getResource(cleaned);
    if (Objects.isNull(fileURL)) {
      // Check if the path is absolute (platform specific)
      if (PathUtil.isAbsolutePath(cleaned)) {
        // For absolute paths, convert directly to URL without resolving against rootFilePath
        fileURL = Paths.get(cleaned).toUri().toURL();
      } else {
        try {
          // Resolve against the root file path using Path to handle '..' properly
          final Path rootPath = Paths.get(rootFilePath);
          final Path base = Files.isDirectory(rootPath) ? rootPath : (rootPath.getParent() != null ? rootPath.getParent() : rootPath);
          final Path resolved = base.resolve(Paths.get(cleaned)).normalize();
          fileURL = resolved.toUri().toURL();
        } catch (final Exception e) {
          // Fallback: resolve the cleaned path against the rootFilePath URI
          final URI resolvedUri = rootFilePath.resolve(cleaned);
          fileURL = resolvedUri.toURL();
        }
      }
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

  // Convierte entradas como snake_case o kebab-case o con espacios a PascalCase (CamelCase con inicial mayúscula)
  private static String toPascalCase(final String input) {
    if (input == null || input.isEmpty()) {
      return input;
    }
    final String[] parts = input.split("[_\\-\\s]+");
    final StringBuilder sb = new StringBuilder();
    for (final String part : parts) {
      if (part == null || part.isEmpty()) {
        continue;
      }
      final String lower = part.toLowerCase();
      sb.append(Character.toUpperCase(lower.charAt(0)));
      if (lower.length() > 1) {
        sb.append(lower.substring(1));
      }
    }
    return sb.toString();
  }
}
