/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import com.sngular.api.generator.plugin.common.tools.StringCaseUtils;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.lang3.StringUtils;

/**
 * Centralized handler for OpenAPI response wrapper generation decisions.
 *
 * Unified architecture (v7.0) that eliminates split logic between OpenApiUtil
 * and MapperPathUtil. This class is the single source of truth for all decisions
 * about inline response wrapper creation, naming, and type resolution.
 *
 * Handles:
 * - Inline objects (create wrapper)
 * - Composed types (allOf, anyOf, oneOf with wrapper)
 * - Arrays with inline items (create wrapper for items)
 * - Arrays with ref items (no wrapper, use ref directly)
 * - Nested arrays (recursive handling)
 * - Edge cases (null, empty, mixed types)
 */
public class ResponseWrapperHandler {

  private ResponseWrapperHandler() {
  }

  /**
   * Determines if a response schema requires creating an inline wrapper class.
   *
   * Rules:
   * - Inline objects → wrapper needed
   * - Composed types (allOf/anyOf/oneOf) → wrapper needed
   * - Refs → no wrapper (use referenced schema)
   * - Arrays with inline items → wrapper needed for items
   * - Arrays with ref items → no wrapper (use List&lt;RefType&gt;)
   * - Nested arrays → recursive check on items
   *
   * @param schema the response schema
   * @return true if wrapper should be created, false if schema can be used directly
   */
  public static boolean shouldCreateWrapper(final JsonNode schema) {
    if (schema == null) {
      return false;
    }

    // Direct ref → no wrapper needed
    if (ApiTool.hasRef(schema)) {
      return false;
    }

    // Composed types (allOf, anyOf, oneOf) → wrapper needed
    if (ApiTool.isComposed(schema)) {
      return true;
    }

    // Inline object → wrapper needed
    if (ApiTool.isObject(schema)) {
      return true;
    }

    // Array type → check items
    if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
      final var items = ApiTool.getItems(schema);

      // Array with ref items → no wrapper (List<RefType>)
      if (ApiTool.hasRef(items)) {
        return false;
      }

      // Array with inline object → wrapper needed for items
      if (ApiTool.isObject(items)) {
        return true;
      }

      // Array with composed type → wrapper needed for items
      if (ApiTool.isComposed(items)) {
        return true;
      }

      // Array with nested array → recursive check
      if (ApiTool.isArray(items)) {
        return shouldCreateWrapper(items);
      }
    }

    // Other types (primitives, etc.) → no wrapper
    return false;
  }

  /**
   * Generates the fully qualified wrapper class name.
   *
   * Naming convention:
   * - InlineResponse{responseCode}{CapitalizedOperationId}
   * - For composed: InlineResponse{responseCode}{CapitalizedOperationId}{ComposedType}
   * - For nested: includes path to nested level
   *
   * @param responseCode HTTP response code (e.g., "200", "201")
   * @param operationId operation ID from OpenAPI spec
   * @param schema the schema being wrapped (may be nested)
   * @param specFile spec configuration
   * @return wrapper class name in snake_case
   */
  public static String getWrapperName(
      final String responseCode, final String operationId, final JsonNode schema, final SpecFile specFile) {
    final String operationIdCap = StringUtils.capitalize(operationId);
    final String composedSuffix = getComposedTypeSuffix(schema);
    final String nestedSuffix = getNestedArraySuffix(schema);

    final String baseName = "InlineResponse" + responseCode + operationIdCap + composedSuffix + nestedSuffix;
    return StringCaseUtils.titleToSnakeCase(MapperUtil.getPojoName(baseName, specFile));
  }

  /**
   * Extracts the actual schema that should be added to the model map.
   *
   * Rules:
   * - Inline object → return the schema itself
   * - Composed → return the schema itself
   * - Array with inline items → return the items (not wrapped in array)
   * - Array with ref items → return null (no model needed, use ref directly)
   * - Nested arrays → recursive extraction
   *
   * @param schema the response schema
   * @return the schema to add to model map, or null if no wrapper needed
   */
  public static JsonNode extractSchemaForModel(final JsonNode schema) {
    if (schema == null) {
      return null;
    }

    // Direct ref → no extraction needed
    if (ApiTool.hasRef(schema)) {
      return null;
    }

    // Inline object or composed → add as-is
    if (ApiTool.isObject(schema) || ApiTool.isComposed(schema)) {
      return schema;
    }

    // Array type
    if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
      final var items = ApiTool.getItems(schema);

      // Ref items → no model needed
      if (ApiTool.hasRef(items)) {
        return null;
      }

      // Inline object or composed items → extract items
      if (ApiTool.isObject(items) || ApiTool.isComposed(items)) {
        return items;
      }

      // Nested array → recurse
      if (ApiTool.isArray(items)) {
        return extractSchemaForModel(items);
      }
    }

    // Other cases → no model needed
    return null;
  }

  /**
   * Returns a list of all wrappers that need to be created for a schema.
   * Handles nested cases where multiple wrappers are needed.
   *
   * Example: Nested array of composed types might need:
   * 1. Wrapper for inner items
   * 2. Wrapper for outer array
   *
   * @param responseCode HTTP response code
   * @param operationId operation ID
   * @param schema the schema
   * @param specFile spec configuration
   * @return list of (name, schema) pairs to add to model map
   */
  public static List<WrapperDefinition> getAllWrappers(
      final String responseCode, final String operationId, final JsonNode schema, final SpecFile specFile) {
    final List<WrapperDefinition> wrappers = new ArrayList<>();
    collectWrappers(responseCode, operationId, schema, specFile, wrappers, "");
    return wrappers;
  }

  private static void collectWrappers(
      final String responseCode, final String operationId, final JsonNode schema, final SpecFile specFile,
      final List<WrapperDefinition> wrappers, final String nesting) {
    if (schema == null || !shouldCreateWrapper(schema)) {
      return;
    }

    // Create wrapper for current level
    final String wrapperName = getWrapperName(responseCode, operationId + nesting, schema, specFile);
    final JsonNode schemaForModel = extractSchemaForModel(schema);

    if (schemaForModel != null) {
      wrappers.add(new WrapperDefinition(wrapperName, schemaForModel));
    }

    // Recurse into nested arrays
    if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
      final var items = ApiTool.getItems(schema);
      if (ApiTool.isArray(items)) {
        collectWrappers(responseCode, operationId, items, specFile, wrappers, nesting + "Array");
      }
    }
  }

  private static String getComposedTypeSuffix(final JsonNode schema) {
    if (schema == null) {
      return "";
    }

    if (Objects.nonNull(schema.findValue("allOf"))) {
      return "AllOf";
    } else if (Objects.nonNull(schema.findValue("anyOf"))) {
      return "AnyOf";
    } else if (Objects.nonNull(schema.findValue("oneOf"))) {
      return "OneOf";
    }

    return "";
  }

  private static String getNestedArraySuffix(final JsonNode schema) {
    if (schema == null || !ApiTool.isArray(schema)) {
      return "";
    }

    if (!ApiTool.hasItems(schema)) {
      return "";
    }

    final var items = ApiTool.getItems(schema);

    // Only add suffix for nested arrays, not for arrays with simple/ref items
    if (ApiTool.isArray(items)) {
      return "Nested";
    }

    return "";
  }

  /**
   * Represents a wrapper class definition to be added to the model map.
   */
  public static class WrapperDefinition {
    private final String name;
    private final JsonNode schema;

    public WrapperDefinition(final String name, final JsonNode schema) {
      this.name = name;
      this.schema = schema;
    }

    public String getName() {
      return name;
    }

    public JsonNode getSchema() {
      return schema;
    }
  }
}
