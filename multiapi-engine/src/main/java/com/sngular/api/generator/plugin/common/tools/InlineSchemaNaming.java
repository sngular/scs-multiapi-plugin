/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.commons.lang3.StringUtils;

/**
 * Names the classes generated for inline schemas. An inline schema has no name of its own, so its
 * class is named after the property that declares it ({@code services} gives {@code Services}). When
 * the same property name declares inline schemas of different shapes in different places, those
 * classes would all be written to one file and every property would share the last shape, silently
 * dropping the fields of the others. Only in that case each class is named after its parent too
 * ({@code NewSalesPoint} + {@code services} gives {@code NewSalesPointServices}), so contracts with no
 * such clash keep their class names.
 *
 * <p>The clashing names are computed from the whole contract before any model is built, so the naming
 * does not depend on the order the schemas are processed in.
 */
public final class InlineSchemaNaming {

  private static final String PROPERTIES = "properties";

  private static final String ITEMS = "items";

  private static final ThreadLocal<Set<String>> CLASHING_NAMES = ThreadLocal.withInitial(Set::of);

  private static final ThreadLocal<Deque<String>> SCHEMAS_BEING_BUILT = ThreadLocal.withInitial(ArrayDeque::new);

  private InlineSchemaNaming() {
  }

  /**
   * Records the inline class names that clash in {@code schemas}, the contract's named schemas keyed
   * as the generator keys them. It holds until {@link #clear()}.
   */
  public static void prepare(final Map<String, JsonNode> schemas) {
    CLASHING_NAMES.set(findClashingNames(schemas));
  }

  public static void clear() {
    CLASHING_NAMES.remove();
    SCHEMAS_BEING_BUILT.remove();
  }

  static Set<String> findClashingNames(final Map<String, JsonNode> schemas) {
    final Map<String, Set<JsonNode>> shapesByName = new HashMap<>();
    for (final Entry<String, JsonNode> schema : schemas.entrySet()) {
      if (describesAClass(schema.getValue())) {
        // A named schema occupies its class name too, so an inline schema of another shape under a
        // property of the same name would overwrite it.
        shapesByName.computeIfAbsent(classBaseName(schema.getKey()), key -> new HashSet<>()).add(schema.getValue());
      }
      collectInlineShapes(schema.getValue(), shapesByName);
    }
    return shapesByName.entrySet().stream()
                       .filter(entry -> entry.getValue().size() > 1)
                       .map(Entry::getKey)
                       .collect(Collectors.toUnmodifiableSet());
  }

  private static void collectInlineShapes(final JsonNode node, final Map<String, Set<JsonNode>> shapesByName) {
    if (Objects.isNull(node) || !node.isContainerNode()) {
      return;
    }
    if (node.isObject() && node.path(PROPERTIES).isObject()) {
      node.get(PROPERTIES).fields().forEachRemaining(property -> {
        final JsonNode inlineSchema = inlineSchemaOf(property.getValue());
        if (Objects.nonNull(inlineSchema)) {
          shapesByName.computeIfAbsent(StringUtils.capitalize(property.getKey()), key -> new HashSet<>()).add(inlineSchema);
        }
      });
    }
    node.elements().forEachRemaining(child -> collectInlineShapes(child, shapesByName));
  }

  /** The inline schema a property gets a class for, itself or its array items, if any. */
  private static JsonNode inlineSchemaOf(final JsonNode property) {
    JsonNode inlineSchema = null;
    if (describesAClass(property)) {
      inlineSchema = property;
    } else if (ApiTool.isArray(property) && describesAClass(property.get(ITEMS))) {
      inlineSchema = property.get(ITEMS);
    }
    return inlineSchema;
  }

  private static boolean describesAClass(final JsonNode schema) {
    return Objects.nonNull(schema) && schema.isObject() && !ApiTool.hasRef(schema) && !ApiTool.isEnum(schema)
           && (ApiTool.hasProperties(schema) || ApiTool.isComposed(schema));
  }

  private static String classBaseName(final String schemaKey) {
    return StringUtils.capitalize(StringUtils.startsWith(schemaKey, "Inline") ? schemaKey : MapperUtil.getKeySchemaName(schemaKey));
  }

  static void enterSchema(final String className) {
    SCHEMAS_BEING_BUILT.get().push(StringUtils.defaultString(className));
  }

  static void exitSchema() {
    SCHEMAS_BEING_BUILT.get().pop();
  }

  /**
   * The class name for an inline schema declared by {@code propertyName} in the schema currently
   * being built: the property name, prefixed with that schema's name when the property name clashes.
   */
  static String inlineClassName(final String propertyName) {
    final Deque<String> schemasBeingBuilt = SCHEMAS_BEING_BUILT.get();
    final String parent = schemasBeingBuilt.isEmpty() ? "" : schemasBeingBuilt.peek();
    return CLASHING_NAMES.get().contains(StringUtils.capitalize(propertyName)) && StringUtils.isNotBlank(parent)
               ? parent + StringUtils.capitalize(propertyName)
               : propertyName;
  }
}
