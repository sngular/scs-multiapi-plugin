package com.sngular.api.generator.plugin.common.tools;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.Test;

class SchemaNormalizationTest {

  @Test
  void toPascalCase_shouldTransformVariousPatterns() throws Exception {
    final Method m = SchemaUtil.class.getDeclaredMethod("toPascalCase", String.class);
    m.setAccessible(true);

    assertEquals("MySchema", m.invoke(null, "my_schema"));
    assertEquals("MySchema", m.invoke(null, "my-schema"));
    assertEquals("MySchema", m.invoke(null, "my schema"));
    assertEquals("Alreadycamel", m.invoke(null, "alreadyCamel"));
    assertEquals("", m.invoke(null, ""));
    assertEquals(null, m.invoke(null, (Object) null));
  }

  @Test
  void solveRef_shouldInsertNormalizedKeysInSchemaMap() throws Exception {
    final Path tmp = Files.createTempDirectory("schema-normalize-test");
    try {
      final Path moduleSub = tmp.resolve("module").resolve("sub");
      Files.createDirectories(moduleSub);
      final Path rootFile = moduleSub.resolve("root.yml");
      Files.writeString(rootFile, "dummy: true");

      final Path apiContract = tmp.resolve("api-contract");
      Files.createDirectories(apiContract);
      final Path openapiFile = apiContract.resolve("openapi.yml");

      final String openapiContent = "components:\n  schemas:\n    my_schema:\n      type: object\n    AnotherName:\n      type: object\n";
      Files.writeString(openapiFile, openapiContent);

      final Map<String, JsonNode> schemaMap = new HashMap<>();

      final String refValue = "..\\..\\api-contract\\openapi.yml#/components/schemas/my_schema";

      final JsonNode result = SchemaUtil.solveRef(refValue, schemaMap, rootFile.toUri());

      // after solveRef, schemaMap must contain both original and PascalCase keys
      assertTrue(schemaMap.containsKey("my_schema"));
      assertTrue(schemaMap.containsKey("MySchema"));

      final JsonNode original = schemaMap.get("my_schema");
      final JsonNode normalized = schemaMap.get("MySchema");

      assertNotNull(original);
      assertNotNull(normalized);
      assertEquals(original, normalized, "La entrada normalizada debe apuntar al mismo nodo que la original");

    } finally {
      try { Files.walk(tmp).map(Path::toFile).sorted((a,b)->-a.compareTo(b)).forEach(java.io.File::delete); } catch (Exception ignored) {}
    }
  }
}

