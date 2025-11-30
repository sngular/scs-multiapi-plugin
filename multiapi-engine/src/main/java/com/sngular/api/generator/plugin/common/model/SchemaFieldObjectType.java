/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.model;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.Map;
import java.util.Objects;

import com.sngular.api.generator.plugin.openapi.exception.CodeGenerationException;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;

@Data
public class SchemaFieldObjectType {

  public static final String ZONED_DATE_TIME = "ZonedDateTime";

  public static final String OFFSET_DATE_TIME = "OffsetDateTime";

  private static final Map<String, String> TYPE_MAPPINGS = Map.ofEntries(
      new SimpleImmutableEntry<>(TypeConstants.OBJECT, "Object"),
      new SimpleImmutableEntry<>(TypeConstants.ARRAY, "List<?>"),
      new SimpleImmutableEntry<>(TypeConstants.MAP, "Map<String, ?>"),
      new SimpleImmutableEntry<>(TypeConstants.BIG_DECIMAL, "BigDecimal"),
      new SimpleImmutableEntry<>(TypeConstants.INTEGER, "Integer"),
      new SimpleImmutableEntry<>(TypeConstants.DOUBLE, "Double"),
      new SimpleImmutableEntry<>(TypeConstants.FLOAT, "Float"),
      new SimpleImmutableEntry<>(TypeConstants.LONG, "Long"),
      new SimpleImmutableEntry<>(TypeConstants.STRING, "String"),
      new SimpleImmutableEntry<>(TypeConstants.ENUM, "Enum"),
      new SimpleImmutableEntry<>(TypeConstants.LOCALDATE, "LocalDate"),
      new SimpleImmutableEntry<>(TypeConstants.LOCALDATETIME, "LocalDateTime"),
      new SimpleImmutableEntry<>(TypeConstants.ZONEDDATE, ZONED_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.ZONEDDATETIME, ZONED_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.OFFSETDATE, OFFSET_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.OFFSETDATETIME, OFFSET_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.MULTIPART_FILE, "MultipartFile")
                                                                        );

  private static final Map<String, String> IMPORT_TYPE_MAPPINGS = Map.ofEntries(
      new SimpleImmutableEntry<>(TypeConstants.OBJECT, "java.util.Object"),
      new SimpleImmutableEntry<>(TypeConstants.ARRAY, "java.util.List"),
      new SimpleImmutableEntry<>(TypeConstants.MAP, "java.util.Map"),
      new SimpleImmutableEntry<>(TypeConstants.BIG_DECIMAL, "java.math.BigDecimal"),
      new SimpleImmutableEntry<>(TypeConstants.STRING, "java.lang.String"),
      new SimpleImmutableEntry<>(TypeConstants.LOCALDATE, "java.time.LocalDate"),
      new SimpleImmutableEntry<>(TypeConstants.LOCALDATETIME, "java.time.LocalDateTime"),
      new SimpleImmutableEntry<>(TypeConstants.ZONEDDATE, "java.time." + ZONED_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.ZONEDDATETIME, "java.time." + ZONED_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.OFFSETDATE, "java.time." + OFFSET_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.OFFSETDATETIME, "java.time." + OFFSET_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.MULTIPART_FILE, "MultipartFile")
                                                                               );

  private static final Map<String, String> IMPL_TYPE_MAPPINGS = Map.ofEntries(
      new SimpleImmutableEntry<>(TypeConstants.OBJECT, "Object"),
      new SimpleImmutableEntry<>(TypeConstants.ARRAY, "ArrayList<?>"),
      new SimpleImmutableEntry<>(TypeConstants.MAP, "HashMap<String, ?>"),
      new SimpleImmutableEntry<>(TypeConstants.BIG_DECIMAL, "BigDecimal"),
      new SimpleImmutableEntry<>(TypeConstants.INTEGER, "Integer"),
      new SimpleImmutableEntry<>(TypeConstants.DOUBLE, "Double"),
      new SimpleImmutableEntry<>(TypeConstants.FLOAT, "Float"),
      new SimpleImmutableEntry<>(TypeConstants.LONG, "Long"),
      new SimpleImmutableEntry<>(TypeConstants.STRING, "String"),
      new SimpleImmutableEntry<>(TypeConstants.ENUM, "Enum"),
      new SimpleImmutableEntry<>(TypeConstants.LOCALDATE, "LocalDate"),
      new SimpleImmutableEntry<>(TypeConstants.LOCALDATETIME, "LocalDateTime"),
      new SimpleImmutableEntry<>(TypeConstants.ZONEDDATE, ZONED_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.ZONEDDATETIME, ZONED_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.OFFSETDATE, OFFSET_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.OFFSETDATETIME, OFFSET_DATE_TIME),
      new SimpleImmutableEntry<>(TypeConstants.MULTIPART_FILE, "MultipartFile")
                                                                             );

  private final String baseType;

  private SchemaFieldObjectType innerType;

  public SchemaFieldObjectType(final String baseType, final SchemaFieldObjectType innerType) {
    this.innerType = innerType;
    this.baseType = baseType;
  }

  public SchemaFieldObjectType(final String type) {
    this.innerType = null;
    this.baseType = type;
  }

  public static SchemaFieldObjectType fromTypeList(final String... types) {
    final SchemaFieldObjectType result = new SchemaFieldObjectType(types[0], null);
    SchemaFieldObjectType objectType = result;

    for (int i = 1; i < types.length; i++) {
      objectType.innerType = new SchemaFieldObjectType(types[i], null);
      objectType = objectType.innerType;
    }

    return result;
  }

  public void setDeepType(final String type) {
    setDeepType(new SchemaFieldObjectType(type));
  }

  public void setDeepType(final SchemaFieldObjectType type) {
    SchemaFieldObjectType parentType = this;
    while (Objects.nonNull(parentType.innerType)) {
      parentType = parentType.innerType;
    }

    parentType.innerType = type;
  }

  public boolean containsType(final String type) {
    return type.equalsIgnoreCase(baseType) || Objects.nonNull(innerType) && innerType.containsType(type);
  }

  private String mapIntoString(final Map<String, String> mappings) {
    final String baseString = TypeConstants.OBJECT.equals(baseType) && Objects.nonNull(innerType) ? "?" : mappings.getOrDefault(baseType, StringUtils.capitalize(baseType));
    final boolean hasInner = baseString.contains("?");

    if (hasInner && Objects.isNull(innerType)) {
      throw new CodeGenerationException(String.format("Field object type '%s' missing an inner type", baseType));
    }

    return hasInner ? baseString.replace("?", innerType.mapIntoString(TYPE_MAPPINGS)) : baseString;
  }

  @SuppressWarnings("unused") // This method is invoked by templates
  public String getImplementationTypeString() {
    return mapIntoString(IMPL_TYPE_MAPPINGS);
  }

  private String innerGetClassString() {
    final String baseString = TypeConstants.OBJECT.equals(baseType) && Objects.nonNull(innerType) ? innerType.innerGetClassString()
                                  : TYPE_MAPPINGS.getOrDefault(baseType, baseType);
    return baseString.split("<")[0];
  }

  @SuppressWarnings("unused") // This method is invoked by templates
  public String getClassString() {
    return innerGetClassString() + ".class";
  }

  @SuppressWarnings("unused") // This method is invoked by templates
  public String getVariableNameString() {
    return StringUtils.uncapitalize(toString().replaceAll("[<>]", ""));
  }

  @Override
  public int hashCode() {
    return Objects.hash(Objects.isNull(innerType) ? 0 : innerType.hashCode(), baseType);
  }

  @Override
  public boolean equals(final Object obj) {
    boolean result = false;
    if (obj instanceof SchemaFieldObjectType other) {
      final boolean baseTypeIsEqual = baseType.equals(other.baseType);
      final boolean innerTypeIsEqual = Objects.isNull(innerType) ? Objects.isNull(other.innerType) : innerType.equals(other.innerType);
      result = baseTypeIsEqual && innerTypeIsEqual;
    }

    return result;
  }

  @Override
  public String toString() {
    return mapIntoString(TYPE_MAPPINGS);
  }

  public String getImportName() {
    return IMPORT_TYPE_MAPPINGS.get(this.baseType);
  }
}
