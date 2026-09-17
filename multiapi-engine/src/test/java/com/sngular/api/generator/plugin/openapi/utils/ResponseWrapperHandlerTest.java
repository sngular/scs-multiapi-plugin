/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

/**
 * Comprehensive test suite for ResponseWrapperHandler (v7.0).
 *
 * Tests all edge cases identified in issue #429 analysis:
 * 1. Inline objects
 * 2. Arrays with ref items
 * 3. Arrays with inline objects
 * 4. Composed types (allOf, anyOf, oneOf)
 * 5. Nested arrays
 * 6. Mixed scenarios
 * 7. Edge cases (null, empty)
 */
@DisplayName("ResponseWrapperHandler - Unified Response Wrapper Logic")
class ResponseWrapperHandlerTest {

  private static final SpecFile specFile = SpecFile.builder().build();

  // ============================================================
  // Case 1: Inline Object Response
  // ============================================================

  @Test
  @DisplayName("Case 1a: Inline object should create wrapper")
  void inlineObjectCreatesWrapper() {
    JsonNode schema = createObjectSchema();

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("Inline object should create wrapper")
        .isTrue();
  }

  @Test
  @DisplayName("Case 1b: Inline object schema should be extracted for model")
  void inlineObjectSchemaExtracted() {
    JsonNode schema = createObjectSchema();
    JsonNode extracted = ResponseWrapperHandler.extractSchemaForModel(schema);

    assertThat(extracted)
        .as("Inline object should be extracted")
        .isNotNull()
        .isEqualTo(schema);
  }

  // ============================================================
  // Case 2: Array with Ref Items
  // ============================================================

  @Test
  @DisplayName("Case 2a: Array with ref items should NOT create wrapper")
  void arrayWithRefNoWrapper() {
    JsonNode schema = createArraySchema(createRefSchema("#/components/schemas/Item"));

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("Array with ref items should not create wrapper (use List<RefType>)")
        .isFalse();
  }

  @Test
  @DisplayName("Case 2b: Array with ref items should NOT extract schema")
  void arrayWithRefNoExtraction() {
    JsonNode schema = createArraySchema(createRefSchema("#/components/schemas/Item"));
    JsonNode extracted = ResponseWrapperHandler.extractSchemaForModel(schema);

    assertThat(extracted)
        .as("Array with ref items should not extract (use ref directly)")
        .isNull();
  }

  // ============================================================
  // Case 3: Array with Inline Object Items
  // ============================================================

  @Test
  @DisplayName("Case 3a: Array with inline object items should create wrapper")
  void arrayWithInlineObjectCreatesWrapper() {
    JsonNode itemSchema = createObjectSchema();
    JsonNode schema = createArraySchema(itemSchema);

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("Array with inline object items should create wrapper for items")
        .isTrue();
  }

  @Test
  @DisplayName("Case 3b: Array with inline object items should extract items")
  void arrayWithInlineObjectExtractItems() {
    JsonNode itemSchema = createObjectSchema();
    JsonNode schema = createArraySchema(itemSchema);
    JsonNode extracted = ResponseWrapperHandler.extractSchemaForModel(schema);

    assertThat(extracted)
        .as("Array with inline items should extract the items (not array wrapper)")
        .isNotNull()
        .isEqualTo(itemSchema);
  }

  // ============================================================
  // Case 4: Composed Types
  // ============================================================

  @Test
  @DisplayName("Case 4a: AllOf should create wrapper")
  void allOfCreatesWrapper() {
    JsonNode schema = createAllOfSchema();

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("AllOf composed type should create wrapper")
        .isTrue();
  }

  @Test
  @DisplayName("Case 4b: AnyOf should create wrapper")
  void anyOfCreatesWrapper() {
    JsonNode schema = createAnyOfSchema();

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("AnyOf composed type should create wrapper")
        .isTrue();
  }

  @Test
  @DisplayName("Case 4c: OneOf should create wrapper")
  void oneOfCreatesWrapper() {
    JsonNode schema = createOneOfSchema();

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("OneOf composed type should create wrapper")
        .isTrue();
  }

  @Test
  @DisplayName("Case 4d: Array with composed items should create wrapper")
  void arrayWithComposedItemsCreatesWrapper() {
    JsonNode itemSchema = createAllOfSchema();
    JsonNode schema = createArraySchema(itemSchema);

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("Array with composed items should create wrapper for items")
        .isTrue();
  }

  // ============================================================
  // Case 5: Nested Arrays
  // ============================================================

  @Test
  @DisplayName("Case 5a: Nested array with inline items should handle recursively")
  void nestedArrayInlineItemsCreatesWrapper() {
    JsonNode innerItems = createObjectSchema();
    JsonNode innerArray = createArraySchema(innerItems);
    JsonNode outerArray = createArraySchema(innerArray);

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(outerArray))
        .as("Nested array should create wrapper recursively")
        .isTrue();
  }

  @Test
  @DisplayName("Case 5b: Nested array should extract correct schema")
  void nestedArrayExtractsCorrectly() {
    JsonNode innerItems = createObjectSchema();
    JsonNode innerArray = createArraySchema(innerItems);
    JsonNode outerArray = createArraySchema(innerArray);

    JsonNode extracted = ResponseWrapperHandler.extractSchemaForModel(outerArray);

    // For nested arrays, we extract recursively
    assertThat(extracted)
        .as("Nested array should recursively extract inner items")
        .isNotNull();
  }

  @Test
  @DisplayName("Case 5c: Nested array with ref items should not create wrapper at top level")
  void nestedArrayRefItemsNoWrapper() {
    JsonNode refSchema = createRefSchema("#/components/schemas/Item");
    JsonNode innerArray = createArraySchema(refSchema);
    JsonNode outerArray = createArraySchema(innerArray);

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(outerArray))
        .as("Nested array with ref items should not create wrapper (recursive ref)")
        .isFalse();
  }

  // ============================================================
  // Case 6: Direct References
  // ============================================================

  @Test
  @DisplayName("Case 6a: Direct ref should NOT create wrapper")
  void directRefNoWrapper() {
    JsonNode schema = createRefSchema("#/components/schemas/Item");

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("Direct reference should not create wrapper")
        .isFalse();
  }

  @Test
  @DisplayName("Case 6b: Direct ref should NOT extract schema")
  void directRefNoExtraction() {
    JsonNode schema = createRefSchema("#/components/schemas/Item");
    JsonNode extracted = ResponseWrapperHandler.extractSchemaForModel(schema);

    assertThat(extracted)
        .as("Direct reference should not extract")
        .isNull();
  }

  // ============================================================
  // Case 7: Naming Consistency
  // ============================================================

  @Test
  @DisplayName("Case 7a: Wrapper names should be consistent")
  void wrapperNamesConsistent() {
    JsonNode schema = createObjectSchema();
    String wrapperName = ResponseWrapperHandler.getWrapperName("200", "listItems", schema, specFile);

    assertThat(wrapperName.toUpperCase())
        .as("Wrapper name should follow naming convention")
        .contains("RESPONSE")
        .contains("LIST")
        .contains("ITEMS");
  }

  @Test
  @DisplayName("Case 7b: Composed type names should include composition suffix")
  void composedTypeNamesIncludeSuffix() {
    JsonNode schema = createAllOfSchema();
    String wrapperName = ResponseWrapperHandler.getWrapperName("200", "getData", schema, specFile);

    assertThat(wrapperName.toUpperCase())
        .as("AllOf wrapper should include 'ALL_OF' suffix in name")
        .contains("ALL_OF")
        .contains("RESPONSE");
  }

  // ============================================================
  // Case 8: getAllWrappers (Comprehensive)
  // ============================================================

  @Test
  @DisplayName("Case 8a: Single inline object returns one wrapper")
  void singleInlineObjectOneWrapper() {
    JsonNode schema = createObjectSchema();
    var wrappers = ResponseWrapperHandler.getAllWrappers("200", "listItems", schema, specFile);

    assertThat(wrappers)
        .as("Single inline object should return one wrapper")
        .hasSize(1);

    assertThat(wrappers.get(0).getSchema())
        .as("Wrapper should contain the schema")
        .isEqualTo(schema);
  }

  @Test
  @DisplayName("Case 8b: Array with ref items returns no wrappers")
  void arrayWithRefNoWrappers() {
    JsonNode schema = createArraySchema(createRefSchema("#/components/schemas/Item"));
    var wrappers = ResponseWrapperHandler.getAllWrappers("200", "listItems", schema, specFile);

    assertThat(wrappers)
        .as("Array with ref items should return no wrappers")
        .isEmpty();
  }

  @Test
  @DisplayName("Case 8c: Array with inline object returns wrapper for items")
  void arrayWithInlineObjectReturnsWrapper() {
    JsonNode itemSchema = createObjectSchema();
    JsonNode schema = createArraySchema(itemSchema);
    var wrappers = ResponseWrapperHandler.getAllWrappers("200", "listItems", schema, specFile);

    assertThat(wrappers)
        .as("Array with inline items should return wrapper for items")
        .hasSize(1);
  }

  // ============================================================
  // Case 9: Edge Cases
  // ============================================================

  @Test
  @DisplayName("Case 9a: Null schema should not create wrapper")
  void nullSchemaNoWrapper() {
    assertThat(ResponseWrapperHandler.shouldCreateWrapper(null))
        .as("Null schema should not create wrapper")
        .isFalse();
  }

  @Test
  @DisplayName("Case 9b: Null schema should not extract")
  void nullSchemaNoExtraction() {
    assertThat(ResponseWrapperHandler.extractSchemaForModel(null))
        .as("Null schema should not extract")
        .isNull();
  }

  @Test
  @DisplayName("Case 9c: Primitive type should not create wrapper")
  void primitiveTypeNoWrapper() {
    JsonNode schema = createPrimitiveSchema("string");

    assertThat(ResponseWrapperHandler.shouldCreateWrapper(schema))
        .as("Primitive type should not create wrapper")
        .isFalse();
  }

  // ============================================================
  // Helper Methods for Schema Creation
  // ============================================================

  private JsonNode createObjectSchema() {
    ObjectNode schema = JsonNodeFactory.instance.objectNode();
    schema.put("type", "object");
    schema.putObject("properties")
        .putObject("id")
        .put("type", "integer")
        .put("description", "Item ID");
    return schema;
  }

  private JsonNode createArraySchema(JsonNode itemsSchema) {
    ObjectNode schema = JsonNodeFactory.instance.objectNode();
    schema.put("type", "array");
    schema.set("items", itemsSchema);
    return schema;
  }

  private JsonNode createRefSchema(String ref) {
    ObjectNode schema = JsonNodeFactory.instance.objectNode();
    schema.put("$ref", ref);
    return schema;
  }

  private JsonNode createAllOfSchema() {
    ObjectNode schema = JsonNodeFactory.instance.objectNode();
    var allOf = schema.putArray("allOf");
    allOf.addObject().put("$ref", "#/components/schemas/Base");
    allOf.addObject().put("$ref", "#/components/schemas/Extended");
    return schema;
  }

  private JsonNode createAnyOfSchema() {
    ObjectNode schema = JsonNodeFactory.instance.objectNode();
    var anyOf = schema.putArray("anyOf");
    anyOf.addObject().put("$ref", "#/components/schemas/Option1");
    anyOf.addObject().put("$ref", "#/components/schemas/Option2");
    return schema;
  }

  private JsonNode createOneOfSchema() {
    ObjectNode schema = JsonNodeFactory.instance.objectNode();
    var oneOf = schema.putArray("oneOf");
    oneOf.addObject().put("$ref", "#/components/schemas/Option1");
    oneOf.addObject().put("$ref", "#/components/schemas/Option2");
    return schema;
  }

  private JsonNode createPrimitiveSchema(String type) {
    ObjectNode schema = JsonNodeFactory.instance.objectNode();
    schema.put("type", type);
    return schema;
  }
}
