package com.sngular.api.generator.plugin.openapi.model;

import java.util.Arrays;
import java.util.Iterator;
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

  public static final Set<String> BASIC_TYPES = Set.of(STRING, INTEGER, OBJECT);

  private static final Map<String, String> typeMappings = Map.of(
      OBJECT, "Object",
      ARRAY, "List<?>",
      MAP, "Map<String, ?>",
      BIG_DECIMAL, "BigDecimal",
      INTEGER, "Integer",
      DOUBLE, "Double",
      FLOAT, "Float",
      LONG, "Long",
      STRING, "String",
      ENUM, "Enum"
  );

  private static final Map<String, String> implTypeMappings = Map.of(
      OBJECT, "Object",
      ARRAY, "ArrayList<?>",
      MAP, "HashMap<String, ?>",
      BIG_DECIMAL, "BigDecimal",
      INTEGER, "Integer",
      DOUBLE, "Double",
      FLOAT, "Float",
      LONG, "Long",
      STRING, "String",
      ENUM, "Enum"
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
    Iterator<String> typeIterator = Arrays.stream(types).iterator();
    return constructTypeFromList(typeIterator);
  }

  private static SchemaFieldObjectType constructTypeFromList(Iterator<String> types) {
    if (!types.hasNext()) {
      return null;
    }

    return new SchemaFieldObjectType(types.next(), constructTypeFromList(types));
  }

  public void setDeepType(SchemaFieldObjectType type) {
    if (Objects.isNull(innerType)) {
      innerType = type;
      return;
    }

    innerType.setDeepType(type);
  }

  public void setDeepType(String type) {
    setDeepType(new SchemaFieldObjectType(type));
  }

  public boolean containsType(String type) {
    return type.equals(baseType) || (Objects.nonNull(innerType) && innerType.containsType(type));
  }

  private String mapIntoString(Map<String, String> mappings) {
    String baseString = mappings.getOrDefault(baseType, baseType);
    if (!baseString.contains("?")) {
      return baseString;
    }

    if (Objects.isNull(innerType)) {
      throw new RuntimeException(String.format("Field object type '%s' missing an inner type", baseType));
    }

    return baseString.replace("?", innerType.mapIntoString(typeMappings));
  }

  public String getImplementationTypeString() {
    return mapIntoString(implTypeMappings);
  }

  @Override
  public String toString() {
    return mapIntoString(typeMappings);
  }

  @Override
  public boolean equals(final Object obj) {
    if (!(obj instanceof SchemaFieldObjectType)) {
      return false;
    }

    SchemaFieldObjectType other = (SchemaFieldObjectType) obj;
    final boolean baseTypeIsEqual = baseType.equals(other.baseType);
    final boolean innerTypeIsEqual = Objects.isNull(innerType) ? Objects.isNull(other.innerType) : innerType.equals(other.innerType);

    return baseTypeIsEqual && innerTypeIsEqual;
  }
}
