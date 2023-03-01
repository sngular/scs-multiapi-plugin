/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiConsumer;

import com.sngular.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.openapi.model.SchemaObject;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {

  private static final String OBJECT = "Object";

  private static final Set<String> BASIC_TYPES = Set.of("String", "Integer", OBJECT);

  private static final String ARRAY = "array";

  private static final String MAP = "map";

  private static final String BIG_DECIMAL = "bigDecimal";

  private static final String INTEGER = "integer";

  private static final String DOUBLE = "double";

  private static final String FLOAT = "float";

  private static final String LONG = "long";

  private static final String STRING = "string";

  private static String schemaCombinatorType;

  private MapperContentUtil() {}

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, Schema> totalSchemas, final Schema<?> schema, final String nameSchema, final SpecFile specFile) {
    return mapComponentToSchemaObject(totalSchemas, new HashMap<>(), new ArrayList<>(), schema, StringUtils.defaultIfBlank(schema.getName(), nameSchema), specFile,
                                      specFile.getModelPackage());
  }

  private static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, Schema> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final Schema<?> schema, final String nameSchema,
      final SpecFile specFile, final String modelPackage) {
    antiLoopList.add(nameSchema);
    final var listSchema = getFields(totalSchemas, schema, specFile, compositedSchemas, antiLoopList);

    compositedSchemas.put(StringUtils.defaultIfBlank(schema.getName(), nameSchema), SchemaObject.builder()
                                                                                                    .schemaName(StringUtils.defaultIfBlank(schema.getName(), nameSchema))
                                                                                                    .className(MapperUtil.getPojoName(nameSchema, specFile))
                                                                                                    .importList(getImportList(listSchema, modelPackage))
                                                                                                    .schemaCombinator(
                                                                                                        StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
                                                                                                    .fieldObjectList(listSchema)
                                                                                                    .build());
    return compositedSchemas;
  }

  private static List<String> getImportList(final List<SchemaFieldObject> fieldObjectList, final String modelPackage) {
    final var listHashMap = new HashMap<String, List<String>>();
    final var importList = new ArrayList<String>();

    for (SchemaFieldObject fieldObject : fieldObjectList) {
      getTypeImports(listHashMap, fieldObject);
      if (StringUtils.isNotBlank(fieldObject.getImportClass()) && !listHashMap.containsKey(fieldObject.getImportClass())) {
        listHashMap.put(StringUtils.capitalize(fieldObject.getImportClass()), List.of(modelPackage + "." + StringUtils.capitalize(fieldObject.getImportClass())));
      }
    }
    if (!listHashMap.isEmpty()) {
      listHashMap.forEach((key, value) -> importList.addAll(value));
    }
    return importList;
  }

  private static void getTypeImports(final HashMap<String, List<String>> listHashMap, final SchemaFieldObject fieldObject) {
    if (Objects.nonNull(fieldObject.getDataTypeSimple())) {
      if (fieldObject.getDataTypeSimple().equalsIgnoreCase(ARRAY)) {
        listHashMap.computeIfAbsent(ARRAY, key -> List.of("java.util.List", "java.util.ArrayList"));
      } else if (Objects.equals(fieldObject.getDataTypeSimple(), MAP)) {
        listHashMap.computeIfAbsent(MAP, key -> List.of("java.util.Map", "java.util.HashMap"));
      } else if (Objects.nonNull(fieldObject.getDataTypeSimple()) && fieldObject.getDataTypeSimple().equalsIgnoreCase(BIG_DECIMAL)
                 || Objects.nonNull(fieldObject.getDataType()) && fieldObject.getDataType().equalsIgnoreCase(BIG_DECIMAL)) {
        listHashMap.computeIfAbsent(BIG_DECIMAL, key -> List.of("java.math.BigDecimal"));
      }
    }
  }

  private static List<SchemaFieldObject> getFields(
      final Map<String, Schema> totalSchemas, final Schema<?> schema, final SpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    schemaCombinatorType = null;
    if (Objects.nonNull(schema.getProperties())) {
      if (Objects.nonNull(schema.getAdditionalProperties())) {
        schema.getProperties().forEach(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
        fieldObjectArrayList.addAll(processFieldObjectList("additionalProperties", null, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
      } else {
        fieldObjectArrayList.addAll(processFieldObjectList(null, null, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
      }
    } else if (ARRAY.equalsIgnoreCase(schema.getType())) {
      final ArraySchema arraySchema = (ArraySchema) schema;
      final String itemType = Objects.nonNull(arraySchema.getItems().get$ref()) ? getRef(arraySchema.getItems(), specFile) : arraySchema.getItems().getType();

      final var field = SchemaFieldObject.builder()
              .baseName("items")
              .dataType(itemType)
              .dataTypeSimple(ARRAY)
              .build();
      fieldObjectArrayList.add(field);
    } else if (Objects.nonNull(schema.getAllOf())) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, schema.getAllOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = "allOf";
    } else if (Objects.nonNull(schema.getAnyOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getAnyOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = "anyOf";
    } else if (Objects.nonNull(schema.getOneOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getOneOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = "oneOf";
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAllOf(
      final Map<String, Schema> totalSchemas, final List<Schema> schemaList, final SpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (Schema<?> ref : schemaList) {
      if (Objects.nonNull(ref.get$ref())) {
        final String[] pathObjectRef = ref.get$ref().split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        fieldObjectArrayList.addAll(processFieldObjectList(null, null, schemaToProcess, specFile, totalSchemas, compositedSchemas, antiLoopList));
        for (var fieldObject : fieldObjectArrayList) {
          fieldObject.setRequired(true);
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAnyOfOneOf(
      final Map<String, Schema> totalSchemas, final List<Schema> schemaList, final SpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (Schema<?> internalSchema : schemaList) {
      if (Objects.nonNull(internalSchema.get$ref())) {
        final String[] pathObjectRef = internalSchema.get$ref().split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        if (compositedSchemas.containsKey(schemaName)) {
          fieldObjectArrayList.add(SchemaFieldObject.builder().baseName(schemaName).dataType(schemaName).build());
        } else {
          fieldObjectArrayList.addAll(processFieldObjectList(null, schemaName, schemaToProcess, specFile, totalSchemas, compositedSchemas, antiLoopList));
        }
      } else {
        for (var fieldObject : fieldObjectArrayList) {
          if (internalSchema.getRequired().contains(fieldObject.getBaseName())) {
            fieldObject.setRequired(true);
          }
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processFieldObjectList(
      final String fieldName, final String className, final Schema<?> schema, final SpecFile specFile,
      final Map<String, Schema> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new LinkedList<SchemaFieldObject>();
    if (ARRAY.equalsIgnoreCase(schema.getType())) {
      fieldObjectArrayList.addAll(processArray(fieldName, className, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
    } else if (schema instanceof MapSchema) {
      if (OBJECT.equalsIgnoreCase(schema.getType())) {
        schema.getProperties().forEach(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
      }
      if (Objects.nonNull(schema.getAdditionalProperties())) {
        if (schema.getAdditionalProperties() instanceof Schema<?>) {
          final Schema<?> additionalProperties = (Schema<?>) schema.getAdditionalProperties();
          if (Objects.nonNull(additionalProperties.get$ref())) {
            final String refSchemaName = getRef(additionalProperties, specFile);
            final var field = SchemaFieldObject.builder()
                    .baseName(fieldName)
                    .dataType(refSchemaName)
                    .dataTypeSimple(MAP)
                    .build();
            setFieldType(field, schema, additionalProperties, specFile, refSchemaName);
            fieldObjectArrayList.add(field);
          } else if (additionalProperties instanceof ObjectSchema || additionalProperties instanceof MapSchema || additionalProperties instanceof ComposedSchema) {
            compositedSchemas.putAll(mapComponentToSchemaObject(totalSchemas, compositedSchemas, antiLoopList, additionalProperties, StringUtils.defaultIfBlank(className, fieldName + "Map"),
                    specFile, specFile.getModelPackage()));
            fieldObjectArrayList
                    .add(SchemaFieldObject
                            .builder()
                            .baseName(fieldName + "Map")
                            .dataType(MapperUtil.getPojoName(fieldName + "Map", specFile))
                            .dataTypeSimple(MAP)
                            .build());
          } else {
            fieldObjectArrayList.addAll(processFieldObjectList(fieldName, className, additionalProperties, specFile, totalSchemas, compositedSchemas, antiLoopList));
          }
        } else {
          fieldObjectArrayList
                  .add(SchemaFieldObject
                          .builder()
                          .baseName(fieldName + "Map")
                          .dataType(MapperUtil.getSimpleType(schema.getAdditionalProperties()))
                          .dataTypeSimple(MAP)
                          .build());
        }
      }
    } else if (Objects.nonNull(schema.get$ref())) {
      final String refSchemaName = getRef(schema, specFile);
      final var field = SchemaFieldObject.builder().baseName(fieldName).dataTypeSimple(MapperUtil.getSimpleType(schema, specFile)).build();
      setFieldType(field, schema, schema, specFile, refSchemaName);
      fieldObjectArrayList.add(field);
    } else if (schema instanceof ObjectSchema || schema instanceof ComposedSchema) {
      if (MapUtils.isEmpty(schema.getProperties())) {
        fieldObjectArrayList.add(SchemaFieldObject.builder().baseName(fieldName).dataTypeSimple(OBJECT).build());
      } else if (schema instanceof ObjectSchema) {
        if (ObjectUtils.allNull(className, fieldName)) {
          schema.getProperties().forEach(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
        } else {
          if (antiLoopList.contains(className)) {
            fieldObjectArrayList
                .add(SchemaFieldObject
                     .builder()
                     .baseName(className)
                     .dataType(MapperUtil.getPojoName(className, specFile))
                     .dataTypeSimple("Object")
                     .build());
          } else if (antiLoopList.contains(fieldName)) {
            fieldObjectArrayList
                .add(SchemaFieldObject
                     .builder()
                     .baseName(fieldName)
                     .dataType(MapperUtil.getPojoName(fieldName, specFile))
                     .dataTypeSimple("Object")
                     .build());
          } else {
            final String name = StringUtils.defaultIfBlank(className, fieldName);
            compositedSchemas.putAll(mapComponentToSchemaObject(totalSchemas, compositedSchemas, antiLoopList, schema, name, specFile,
                                                                specFile.getModelPackage()));
            fieldObjectArrayList
                .add(SchemaFieldObject
                     .builder()
                     .baseName(name)
                     .dataType(MapperUtil.getPojoName(name, specFile))
                     .dataTypeSimple("Object")
                     .build());
          }
        }
      } else {
        final var composedSchemaName = StringUtils.defaultIfBlank(className, fieldName);
        var schemaObjectComposed = compositedSchemas.get(composedSchemaName);
        if (Objects.isNull(schemaObjectComposed)) {
          schemaObjectComposed = createComposedSchema(StringUtils.defaultIfBlank(className, fieldName), schema, specFile, totalSchemas, compositedSchemas, antiLoopList);
          compositedSchemas.put(composedSchemaName, schemaObjectComposed);
        }
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(schemaObjectComposed.getClassName())
                                     .dataTypeSimple(schemaObjectComposed.getClassName())
                                     .build());
      }
    } else {
      final var field = SchemaFieldObject.builder().baseName(fieldName).dataTypeSimple(MapperUtil.getSimpleType(schema, specFile)).build();
      setFieldType(field, schema, schema, specFile, "");
      fieldObjectArrayList.add(field);
    }
    return fieldObjectArrayList;
  }

  private static BiConsumer<String, Schema> processProperties(
      final Map<String, Schema> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final List<SchemaFieldObject> fieldObjectArrayList, final SpecFile specFile, final Schema<?> schema, final List<String> antiLoopList) {
    return (key, value) -> {
      final var enumValues = value.getEnum();
      if (CollectionUtils.isNotEmpty(enumValues)) {
        processEnumField(key, value, specFile, fieldObjectArrayList, enumValues, schema);
      } else {
        fieldObjectArrayList.addAll(processObjectProperty(totalSchemas, key, value, compositedSchemas, specFile, schema, antiLoopList));
      }
    };
  }

  private static List<SchemaFieldObject> processObjectProperty(
      final Map<String, Schema> totalSchemas, final String key, final Schema value,
      final Map<String, SchemaObject> compositedSchemas, final SpecFile specFile, final Schema<?> schema, final List<String> antiLoopList) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();
    final SchemaFieldObject field;
    if (Objects.nonNull(value.get$ref())) {
      final var typeName = cleanRefName(value);
      if (!antiLoopList.contains(typeName) &&
          ((totalSchemas.containsKey(typeName) && Objects.nonNull(totalSchemas.get(typeName).getType()) && totalSchemas.get(typeName).getType().equalsIgnoreCase(ARRAY)) ||
           value.get$ref().contains(key))) {
        antiLoopList.add(typeName);
        fieldObjectArrayList.addAll(processFieldObjectList(key, typeName, totalSchemas.get(typeName), specFile, totalSchemas, compositedSchemas, antiLoopList));
      } else {
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(key)
                                     .dataType(MapperUtil.getPojoName(typeName, specFile))
                                     .dataTypeSimple(MapperUtil.getSimpleType(totalSchemas.get(typeName), specFile))
                                     .build());
      }
    } else if (isBasicType(value)) {
      field = SchemaFieldObject.builder().baseName(key).dataTypeSimple(MapperUtil.getSimpleType(value, specFile)).build();
      setFieldType(field, value, schema, specFile, key);
      fieldObjectArrayList.add(field);
    } else {
      fieldObjectArrayList.addAll(processFieldObjectList(key, key, value, specFile, totalSchemas, compositedSchemas, antiLoopList));
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processArray(
      final String fieldName, final String className, final Schema<?> schema, final SpecFile specFile,
      final Map<String, Schema> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();
    if (Objects.nonNull(schema.getItems())) {
      final ArraySchema arraySchema = (ArraySchema) schema;
      final var items = arraySchema.getItems();
      if (Objects.nonNull(items.get$ref())) {
        final String refSchemaName = getRef(items, specFile);
        final var field = SchemaFieldObject.builder().baseName(fieldName).dataTypeSimple(ARRAY).dataType(MapperUtil.getSimpleType(arraySchema.getItems(), specFile)).build();
        setFieldType(field, arraySchema.getItems(), arraySchema.getItems(), specFile, refSchemaName);
        fieldObjectArrayList.add(field);
      } else if (ObjectUtils.anyNotNull(items.getAnyOf(), items.getAllOf(), items.getOneOf())) {
        final var composedSchemaName = StringUtils.defaultIfBlank(className, fieldName);
        var schemaObjectComposed = compositedSchemas.get(composedSchemaName);
        if (Objects.isNull(schemaObjectComposed)) {
          schemaObjectComposed = createComposedSchema(StringUtils.defaultIfBlank(className, fieldName), items, specFile,
                                                      totalSchemas, compositedSchemas, antiLoopList);
          compositedSchemas.put(composedSchemaName, schemaObjectComposed);
        }
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(schemaObjectComposed.getClassName())
                                     .dataTypeSimple(ARRAY)
                                     .importClass(schemaObjectComposed.getClassName())
                                     .build());
      } else if (Objects.nonNull(items.getProperties())) {
        compositedSchemas.putAll(mapComponentToSchemaObject(totalSchemas, items, fieldName, specFile));
        fieldObjectArrayList.add(SchemaFieldObject.builder().baseName(fieldName).dataType(MapperUtil.getPojoName(fieldName, specFile)).dataTypeSimple(ARRAY).build());
      } else {
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(MapperUtil.getSimpleType(arraySchema.getItems(), specFile))
                                     .dataTypeSimple(ARRAY)
                                     .build());
      }
    } else {
      fieldObjectArrayList.add(SchemaFieldObject.builder().baseName(fieldName).dataType(OBJECT).dataTypeSimple(ARRAY).build());
    }
    return fieldObjectArrayList;
  }

  private static boolean isBasicType(final Schema value) {
    return !(value instanceof ArraySchema || value instanceof ObjectSchema || value instanceof ComposedSchema);
  }

  private static SchemaObject createComposedSchema(
      final String fieldName, final Schema<?> schema, final SpecFile specFile, final Map<String, Schema> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    if (Objects.nonNull(schema.getAllOf())) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, schema.getAllOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = "allOf";
    } else if (Objects.nonNull(schema.getAnyOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getAnyOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = "anyOf";
    } else if (Objects.nonNull(schema.getOneOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getOneOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = "oneOf";
    }

    return SchemaObject.builder()
                       .schemaName(fieldName)
                       .className(MapperUtil.getPojoName(fieldName, specFile))
                       .importList(getImportList(fieldObjectArrayList, specFile.getModelPackage()))
                       .schemaCombinator(StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
                       .fieldObjectList(fieldObjectArrayList)
                       .build();
  }

  private static void setFieldType(final SchemaFieldObject field, final Schema<?> value, final Schema<?> schema, final SpecFile specFile, final String key) {
    field.setRequired(Objects.nonNull(schema.getRequired()) && schema.getRequired().contains(key));
    if (Objects.nonNull(value.getType()) && ARRAY.equalsIgnoreCase(value.getType())) {
      final String typeArray;
      if (Objects.nonNull(value.getItems())) {
        typeArray = MapperUtil.getTypeArray((ArraySchema) value, specFile);
      } else {
        typeArray = OBJECT;
      }
      field.setDataType(typeArray);
      field.setImportClass(getImportClass(typeArray));
      field.setDataTypeSimple(ARRAY);
    } else if (value instanceof MapSchema) {
      if (Objects.nonNull(value.getAdditionalProperties())) {
        field.setDataTypeSimple(MAP);
        final String typeObject = getMapTypeObject(value, specFile);
        field.setDataType(typeObject);
        field.setImportClass(getImportClass(typeObject));
      } else {
        final var typeMap = MapperUtil.getTypeMap((MapSchema) value, specFile);
        field.setDataTypeSimple(MAP);
        field.setDataType(typeMap);
        field.setImportClass(getImportClass(typeMap));
      }
    } else if (Objects.nonNull(value.getType()) && OBJECT.equalsIgnoreCase(value.getType())) {
      var typeObject = "";
      if (StringUtils.isNotBlank(value.get$ref())) {
        typeObject = getRef(schema, specFile);
      }
      field.setImportClass(getImportClass(typeObject));
      field.setDataType(typeObject);
    }
  }

  private static String getMapTypeObject(final Schema schema, final SpecFile specFile) {
    final String typeObject;
    if (schema.getAdditionalProperties() instanceof Boolean && (Boolean) schema.getAdditionalProperties()) {
      typeObject = OBJECT;
    } else {
      final Schema additionalProperties = (Schema) schema.getAdditionalProperties();
      if (StringUtils.isNotBlank(additionalProperties.get$ref())) {
        typeObject = getRef(additionalProperties, specFile);
      } else if (StringUtils.isNotBlank(additionalProperties.getType()) && !additionalProperties.getType().equalsIgnoreCase("object")) {
        final var additionalPropertiesField =
            SchemaFieldObject.builder().baseName(additionalProperties.getName()).dataTypeSimple(MapperUtil.getSimpleType(additionalProperties, specFile)).build();
        setFieldType(additionalPropertiesField, additionalProperties, additionalProperties, specFile, "");
        typeObject = getMapFieldType(additionalPropertiesField);
      } else {
        typeObject = OBJECT;
      }
    }
    return typeObject;
  }

  private static String getMapFieldType(final SchemaFieldObject schemaFieldObject) {
    final String fieldType;
    switch (StringUtils.uncapitalize(schemaFieldObject.getDataTypeSimple())) {
      case BIG_DECIMAL:
      case INTEGER:
      case DOUBLE:
      case FLOAT:
      case LONG:
      case STRING:
        fieldType = StringUtils.capitalize(schemaFieldObject.getDataTypeSimple());
        break;
      default:
        fieldType = OBJECT;
    }
    return fieldType;
  }

  private static String getRef(final Schema<?> schema, final SpecFile specFile) {
    final String typeObject;
    typeObject = MapperUtil.getPojoName(cleanRefName(schema), specFile);
    return typeObject;
  }

  private static String cleanRefName(final Schema<?> schema) {
    final String[] pathObjectRef = schema.get$ref().split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  private static void processEnumField(
      final String key, final Schema<?> value, final SpecFile specFile, final List<SchemaFieldObject> fieldObjectArrayList,
      final List<?> enumValues, final Schema<?> schema) {
    final var field = SchemaFieldObject.builder().baseName(key).dataTypeSimple("enum").build();
    field.setRequired(Objects.nonNull(schema.getRequired()) && schema.getRequired().contains(key));
    final var dataType = MapperUtil.getSimpleType(value, specFile);
    field.setDataType(dataType);

    final HashMap<String, String> enumValuesMap = new HashMap<>();

    for (var enumValue : enumValues) {
      String valueName = enumValue.toString();
      valueName = valueName.replace(".", "_DOT_");

      switch (StringUtils.uncapitalize(dataType)) {
        case INTEGER:
          enumValuesMap.put("INTEGER_" + valueName, enumValue.toString());
          break;
        case LONG:
          enumValuesMap.put("LONG_" + valueName, enumValue + "l");
          break;
        case DOUBLE:
          enumValuesMap.put("DOUBLE_" + valueName, enumValue.toString());
          break;
        case FLOAT:
          enumValuesMap.put("FLOAT_" + valueName, enumValue + "f");
          break;
        case BIG_DECIMAL:
          enumValuesMap.put("BIG_DECIMAL_" + valueName, "new BigDecimal(\"" + enumValue + "\")");
          break;
        case STRING:
        default:
          enumValuesMap.put(StringUtils.upperCase(valueName), '"' + enumValue.toString() + '"');
          break;
      }
    }

    if (!enumValuesMap.isEmpty()) {
      field.setEnumValues(enumValuesMap);
      fieldObjectArrayList.add(field);
    } else {
      throw new BadDefinedEnumException(key);
    }
  }

  private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !BASIC_TYPES.contains(type) ? type : "";
  }

}
