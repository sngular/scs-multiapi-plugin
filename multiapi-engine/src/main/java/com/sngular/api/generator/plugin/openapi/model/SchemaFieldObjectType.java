/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.Map;
import java.util.Objects;

import lombok.Data;

import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.ARRAY;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.MAP;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.BIG_DECIMAL;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.OBJECT;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.ENUM;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.INTEGER;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.LONG;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.FLOAT;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.DOUBLE;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.STRING;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.LOCALDATE;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.LOCALDATETIME;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.ZONEDDATE;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.ZONEDDATETIME;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.OFFSETDATE;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.OFFSETDATETIME;

@Data
public class SchemaFieldObjectType {

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
      new SimpleImmutableEntry<>(LOCALDATE, "LocalDate"),
      new SimpleImmutableEntry<>(LOCALDATETIME, "LocalDateTime"),
      new SimpleImmutableEntry<>(ZONEDDATE, "ZonedDateTime"),
      new SimpleImmutableEntry<>(ZONEDDATETIME, "ZonedDateTime"),
      new SimpleImmutableEntry<>(OFFSETDATE, "OffsetDateTime"),
      new SimpleImmutableEntry<>(OFFSETDATETIME, "OffsetDateTime")
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
      new SimpleImmutableEntry<>(LOCALDATE, "LocalDate"),
      new SimpleImmutableEntry<>(LOCALDATETIME, "LocalDateTime"),
      new SimpleImmutableEntry<>(ZONEDDATE, "ZonedDateTime"),
      new SimpleImmutableEntry<>(ZONEDDATETIME, "ZonedDateTime"),
      new SimpleImmutableEntry<>(OFFSETDATE, "OffsetDateTime"),
      new SimpleImmutableEntry<>(OFFSETDATETIME, "OffsetDateTime")
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
    while (Objects.nonNull(parentType.innerType)) {
      parentType = parentType.innerType;
    }

    parentType.innerType = type;
  }

  public void setDeepType(String type) {
    setDeepType(new SchemaFieldObjectType(type));
  }

  public boolean containsType(String type) {
    return type.equalsIgnoreCase(baseType) || (Objects.nonNull(innerType) && innerType.containsType(type));
  }

  private String mapIntoString(Map<String, String> mappings) {
    final String baseString = mappings.getOrDefault(baseType, baseType);
    final boolean hasInner = baseString.contains("?");

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
      final SchemaFieldObjectType other = (SchemaFieldObjectType) obj;
      final boolean baseTypeIsEqual = baseType.equals(other.baseType);
      final boolean innerTypeIsEqual = Objects.isNull(innerType) ? Objects.isNull(other.innerType) : innerType.equals(other.innerType);
      result = baseTypeIsEqual && innerTypeIsEqual;
    }

    return result;
  }
}
