package com.sngular.api.generator.plugin.common.tools;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.openapi.exception.FileParseException;
import org.apache.commons.lang3.StringUtils;

public class SchemaUtil {

  static final ObjectMapper PARSER = new ObjectMapper(new YAMLFactory());

  protected SchemaUtil() {
  }

  public static JsonNode solveRef(final String refValue, final Map<String, JsonNode> schemaMap, final URI rootFilePath) {
    return solveRef(refValue, schemaMap, rootFilePath, null);
  }

  public static JsonNode solveRef(final String refValue, final Map<String, JsonNode> schemaMap, final URI rootFilePath, final URLClassLoader jarLoader) {
    if (jarLoader != null && !refValue.startsWith("#")) {
      final JsonNode jarRef = solveRefFromJar(refValue, jarLoader);
      if (jarRef != null) {
        return jarRef;
      }
    }

    JsonNode solvedRef;
    if (StringUtils.isNotEmpty(refValue)) {
      if (refValue.startsWith("#")) {
        final String refSchemaName = MapperUtil.getRefSchemaKey(refValue);
        solvedRef = schemaMap.get(refSchemaName);
      } else {
        final var refValueArr = refValue.split("#");
        final var filePath = refValueArr[0];
        final URI actualFileBase = resolveActualBaseUri(rootFilePath, filePath);
        solvedRef = loadAndResolveRefs(rootFilePath, filePath);
        if (ApiTool.hasComponents(solvedRef)) {
          schemaMap.putAll(ApiTool.getComponentSchemas(solvedRef));
          if (refValueArr.length > 1) {
            solvedRef = solvedRef.findValue(MapperUtil.getKey(refValueArr[1]));
          }
        }
      }
    } else {
      solvedRef = null;
    }
    return solvedRef;
  }

  private static JsonNode solveRefFromJar(final String refValue, final URLClassLoader jarLoader) {
    try (InputStream stream = jarLoader.getResourceAsStream(refValue)) {
      if (stream == null) {
        return null;
      }
      return PARSER.readTree(stream);
    } catch (final IOException e) {
      return null;
    }
  }

  /**
   * Computes a clean schema-map key from a file-based $ref value.
   * E.g. "./ServiceType.yml" -> "SCHEMAS/SERVICE_TYPE"
   */
  public static String computeFileSchemaKey(final String refValue) {
    try {
      final String[] parts = refValue.split("/");
      final String lastRaw = parts[parts.length - 1];
      String lastName = lastRaw;
      if (lastName.contains(".")) {
        lastName = lastName.substring(0, lastName.indexOf('.'));
      }
      String category = parts.length >= 2 ? parts[parts.length - 2] : "schemas";
      if (".".equals(category) || "..".equals(category)) {
        category = "schemas";
      }
      return StringUtils.upperCase(category + "/" + StringCaseUtils.titleToSnakeCase(lastName));
    } catch (final Exception e) {
      return null;
    }
  }

  private static URI resolveActualBaseUri(final URI rootFilePath, final String filePath) {
    try {
      final String cleaned = cleanUpPath(filePath).replace('\\', '/');
      final Path rootPath = Paths.get(rootFilePath);
      if (Files.exists(rootPath) && Files.isDirectory(rootPath)) {
        return rootPath.resolve(cleaned).normalize().getParent().toUri();
      }
      final Path parent = rootPath.getParent();
      if (Objects.nonNull(parent)) {
        return parent.resolve(cleaned).normalize().getParent().toUri();
      }
      return null;
    } catch (final Exception e) {
      return null;
    }
  }

  static void resolveNestedFileRefs(final JsonNode node, final URI baseUri) {
    if (Objects.isNull(node)) {
      return;
    }
    if (node.isArray()) {
      resolveNestedFileRefsInArray((ArrayNode) node, baseUri);
    } else if (node.isObject()) {
      resolveNestedFileRefsInObject(node, baseUri);
    }
  }

  private static void resolveNestedFileRefsInArray(final ArrayNode array, final URI baseUri) {
    for (int i = 0; i < array.size(); i++) {
      final JsonNode element = array.get(i);
      if (element.isObject() && element.has("$ref")) {
        final boolean resolved = resolveRefInArray(array, i, baseUri);
        if (!resolved) {
          resolveNestedFileRefs(element, baseUri);
        }
      } else {
        resolveNestedFileRefs(element, baseUri);
      }
    }
  }

  private static void resolveNestedFileRefsInObject(final JsonNode node, final URI baseUri) {
    final Iterator<Entry<String, JsonNode>> fields = node.fields();
    while (fields.hasNext()) {
      final Entry<String, JsonNode> field = fields.next();
      if (field.getValue().isObject() && field.getValue().has("$ref")) {
        final boolean resolved = resolveRefInObject((ObjectNode) node, field.getKey(), baseUri);
        if (!resolved) {
          resolveNestedFileRefs(field.getValue(), baseUri);
        }
      } else {
        resolveNestedFileRefs(field.getValue(), baseUri);
      }
    }
  }

  private static boolean resolveRefInArray(final ArrayNode array, final int index, final URI baseUri) {
    final JsonNode refNode = array.get(index);
    final String refVal = refNode.get("$ref").textValue();
    if (StringUtils.isEmpty(refVal) || refVal.startsWith("#") || refVal.startsWith("http") || PathUtil.isRemoteUri(refVal)) {
      return false;
    }
    try {
      final URI nestedBase = resolveActualBaseUri(baseUri, refVal);
      final JsonNode resolved = resolveExternalFileRef(baseUri, refVal);
      if (Objects.nonNull(resolved)) {
        array.set(index, resolved);
        resolveNestedFileRefs(resolved, nestedBase);
        return true;
      }
    } catch (final Exception ignored) {
    }
    return false;
  }

  private static boolean resolveRefInObject(final ObjectNode parent, final String fieldName, final URI baseUri) {
    final JsonNode refNode = parent.get(fieldName);
    final String refVal = refNode.get("$ref").textValue();
    if (StringUtils.isEmpty(refVal) || refVal.startsWith("#") || refVal.startsWith("http") || PathUtil.isRemoteUri(refVal)) {
      return false;
    }
    try {
      final URI nestedBase = resolveActualBaseUri(baseUri, refVal);
      final JsonNode resolved = resolveExternalFileRef(baseUri, refVal);
      if (Objects.nonNull(resolved)) {
        parent.set(fieldName, resolved);
        resolveNestedFileRefs(resolved, nestedBase);
        return true;
      }
    } catch (final Exception ignored) {
    }
    return false;
  }

  static URI resolveActualBaseUriPublic(final URI rootFilePath, final String filePath) {
    return resolveActualBaseUri(rootFilePath, filePath);
  }

  /**
   * Resolves a reference that points at another file, optionally with a JSON Pointer
   * fragment (e.g. {@code ../components/components.yaml#/components/schemas/Example}).
   * The file itself is loaded relative to {@code baseUri} independent of the fragment, so
   * refs living in an external Path Item file resolve against the folder of that file
   * instead of the root contract.
   *
   * @param baseUri base URI of the file that contains the reference.
   * @param refValue the raw {@code $ref} value.
   * @return the resolved node (whole file, or the fragment target) or {@code null}.
   */
  private static JsonNode resolveExternalFileRef(final URI baseUri, final String refValue) {
    JsonNode resolved = null;
    if (StringUtils.isNotEmpty(refValue)) {
      final var refValueArr = refValue.split("#");
      if (refValueArr.length > 1) {
        final var fileNode = loadAndResolveRefs(baseUri, refValueArr[0]);
        if (ApiTool.hasComponents(fileNode)) {
          resolved = fileNode.findValue(MapperUtil.getKey(refValueArr[1]));
        }
      } else {
        resolved = loadAndResolveRefs(baseUri, refValue);
      }
    }
    return resolved;
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

  public static JsonNode loadAndResolveRefs(final URI rootFilePath, final String refPath) {
    final JsonNode node = getPojoFromRef(rootFilePath, refPath);
    final URI actualBase = resolveActualBaseUri(rootFilePath, refPath);
    if (Objects.nonNull(actualBase)) {
      resolveNestedFileRefs(node, actualBase);
    }
    return node;
  }

  private static String readFile(final URI rootFilePath, final String filePath) throws MalformedURLException {
    if (Objects.isNull(filePath)) {
      throw new IllegalArgumentException("File Path cannot be empty");
    }

    // Remote specifications (http/https/ftp/file URLs, e.g. an Apicurio Registry artifact) are
    // fetched directly from their URL, bypassing classpath and filesystem resolution.
    if (PathUtil.isRemoteUri(filePath)) {
      return readFromUrl(URI.create(filePath).toURL());
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
          // Try to resolve against the root file path using URI resolution
          final URI resolvedUri = resolveFileUri(rootFilePath, cleaned);
          fileURL = resolvedUri.toURL();
        } catch (final Exception e) {
          // Fallback: resolve the cleaned path against the rootFilePath URI (original behavior)
          final URI resolvedUri = rootFilePath.resolve(cleaned);
          fileURL = resolvedUri.toURL();
        }
      }
    }
    return readFromUrl(fileURL);
  }

  /**
   * Resolves a relative path against a base URI, handling JAR URIs for external references.
   * For filesystem URIs, falls back to original behavior.
   *
   * @param baseUri the base URI (may be jar:file:/...!/ or file:///)
   * @param relativePath the relative path to resolve (e.g., "fragments.yml")
   * @return the resolved URI
   */
  private static URI resolveFileUri(final URI baseUri, final String relativePath) {
    if ("jar".equals(baseUri.getScheme())) {
      return resolveJarPath(baseUri, relativePath);
    }
    // For filesystem and other URIs, use standard URI resolution
    return baseUri.resolve(relativePath);
  }

  /**
   * Resolves a relative path within a JAR URI.
   * Example: jar:file:/app/lib.jar!/com/example/ + "fragments.yml" → jar:file:/app/lib.jar!/com/example/fragments.yml
   */
  private static URI resolveJarPath(final URI jarBaseUri, final String relativePath) {
    String ssp = jarBaseUri.getSchemeSpecificPart();
    String[] parts = ssp.split("!", 2);

    if (parts.length != 2) {
      throw new IllegalArgumentException("Invalid JAR URI: " + jarBaseUri);
    }

    String jarPath = parts[0];
    Path basePath = Paths.get(parts[1]);
    Path resolved = basePath.resolve(relativePath).normalize();

    return URI.create("jar:" + jarPath + "!" + toJarEntryPath(resolved));
  }

  /**
   * Converts a Path to a JAR-entry-safe path string using '/' separators.
   */
  private static String toJarEntryPath(final Path path) {
    int nameCount = path.getNameCount();

    if (nameCount == 0) {
      return "/";
    }

    StringBuilder sb = new StringBuilder(path.toString().length() + nameCount + 2);
    sb.append('/');

    for (int i = 0; i < nameCount; i++) {
      sb.append(path.getName(i));
      if (i < nameCount - 1) {
        sb.append('/');
      }
    }

    return sb.toString();
  }

  private static String readFromUrl(final URL fileURL) {
    final var sb = new StringBuilder();
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(PathUtil.openUrlStream(fileURL)))) {
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
