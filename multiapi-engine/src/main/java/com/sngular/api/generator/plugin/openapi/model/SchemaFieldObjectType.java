/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import lombok.Data;

@Data
public class SchemaFieldObjectType {

  public static final String OBJECT = "Object";

  public static final String ARRAY = "Array";

  public static final String MAP = "Map";

  public static final String BIG_DECIMAL = "BigDecimal";

  public static final String INTEGER = "Integer";

  public static final String DOUBLE = "Double";

  public static final String FLOAT = "Float";

  public static final String LONG = "Long";

  public static final String STRING = "String";

  public static final String ENUM = "Enum";

  public static final String DATE = "Date";

  public static final String DATETIME = "DateTime";

  public static final Set<String> BASIC_TYPES = Set.of(STRING, INTEGER, OBJECT);

  private static final Map<String, String> typeMappings = Map.ofEntries(
      new SimpleImmutableEntry<>(OBJECT, "Object"),
      new SimpleImmutableEntry<>(ARRAY, "List<?>"),
      new SimpleImmutableEntry<>(MAP, "Map<String, ?>"),
      new SimpleImmutableEntry<>(BIG_DECIMAL, "BigDecimal"),
      new SimpleImmutableEntry<>(INTEGER, "Integer"),
      new SimpleImmutableEntry<>(DOUBLE, "Double"),
      new SimpleImmutableEntry<>(FLOAT, "Float"),
      new SimpleImmutableEntry<>(LONG, "Long"),
      new SimpleImmutableEntry<>(STRING, "String"),
      new SimpleImmutableEntry<>(ENUM, "Enum"),
      new SimpleImmutableEntry<>(DATE, "LocalDate"),
      new SimpleImmutableEntry<>(DATETIME, "LocalDateTime")
  );

  private static final Map<String, String> implTypeMappings = Map.ofEntries(
      new SimpleImmutableEntry<>(OBJECT, "Object"),
      new SimpleImmutableEntry<>(ARRAY, "ArrayList<?>"),
      new SimpleImmutableEntry<>(MAP, "HashMap<String, ?>"),
      new SimpleImmutableEntry<>(BIG_DECIMAL, "BigDecimal"),
      new SimpleImmutableEntry<>(INTEGER, "Integer"),
      new SimpleImmutableEntry<>(DOUBLE, "Double"),
      new SimpleImmutableEntry<>(FLOAT, "Float"),
      new SimpleImmutableEntry<>(LONG, "Long"),
      new SimpleImmutableEntry<>(STRING, "String"),
      new SimpleImmutableEntry<>(ENUM, "Enum"),
      new SimpleImmutableEntry<>(DATE, "LocalDate"),
      new SimpleImmutableEntry<>(DATETIME, "LocalDateTime")
  );

  private SchemaFieldObjectType innerType;

  private final String baseType;

  public SchemaFieldObjectType(String baseType, SchemaFieldObjectType innerType) {
    this.innerType = innerType;
    this.baseType = baseType;
  }

  public SchemaFieldObjectType(String type) {
    this.innerType = null;
    this.baseType = type;
  }

  public static SchemaFieldObjectType fromTypeList(String... types) {
    final SchemaFieldObjectType result = new SchemaFieldObjectType(types[0], null);
    SchemaFieldObjectType objectType = result;

    for (int i = 1; i < types.length; i++) {
      objectType = objectType.innerType = new SchemaFieldObjectType(types[i], null);
    }

    return result;
  }

  public void setDeepType(SchemaFieldObjectType type) {
    SchemaFieldObjectType parentType = this;
    while(Objects.nonNull(parentType.innerType)) {
      parentType = parentType.innerType;
    }

    parentType.innerType = type;
  }

  public void setDeepType(String type) {
    setDeepType(new SchemaFieldObjectType(type));
  }

  public boolean containsType(String type) {
    return type.equals(baseType) || (Objects.nonNull(innerType) && innerType.containsType(type));
  }

  private String mapIntoString(Map<String, String> mappings) {
    String baseString = mappings.getOrDefault(baseType, baseType);
    boolean hasInner = baseString.contains("?");

    if (hasInner && Objects.isNull(innerType)) {
      throw new RuntimeException(String.format("Field object type '%s' missing an inner type", baseType));
    }

    return hasInner ? baseString.replace("?", innerType.mapIntoString(typeMappings)) : baseString;
  }

  @SuppressWarnings("unused") // This method is invoked by templates
  public String getImplementationTypeString() {
    return mapIntoString(implTypeMappings);
  }

  @Override
  public String toString() {
    return mapIntoString(typeMappings);
  }

  @Override
  public boolean equals(final Object obj) {
    boolean result = false;
    if (obj instanceof SchemaFieldObjectType) {
      SchemaFieldObjectType other = (SchemaFieldObjectType) obj;
      final boolean baseTypeIsEqual = baseType.equals(other.baseType);
      final boolean innerTypeIsEqual = Objects.isNull(innerType) ? Objects.isNull(other.innerType) : innerType.equals(other.innerType);
      result = baseTypeIsEqual && innerTypeIsEqual;
    }

    return result;
  }
}
