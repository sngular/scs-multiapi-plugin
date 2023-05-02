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
import java.util.function.BiConsumer;

import com.sngular.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.openapi.model.SchemaObject;
import com.sngular.api.generator.plugin.openapi.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.DateSchema;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {

  private static final String ADDITIONAL_PROPERTY_NAME = "AdditionalProperty";

  private static final String ANY_OF_COMBINATOR = "anyOf";

  private static final String ONE_OF_COMBINATOR = "oneOf";

  private static final String ALL_OF_COMBINATOR = "allOf";

  private MapperContentUtil() {
  }

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, Schema> totalSchemas, final Schema<?> schema, final String nameSchema, final SpecFile specFile) {
    return mapComponentToSchemaObject(totalSchemas, new HashMap<>(), new ArrayList<>(), schema, StringUtils.defaultIfBlank(schema.getName(), nameSchema), specFile,
                                      specFile.getModelPackage());
  }

  private static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, Schema> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final Schema<?> schema, final String nameSchema,
      final SpecFile specFile, final String modelPackage) {
    antiLoopList.add(nameSchema);

    if (Objects.isNull(schema.getName())) {
      schema.setName(nameSchema);
    }
    final var listSchema = getFields(totalSchemas, schema, specFile, compositedSchemas, antiLoopList);

    String schemaCombinatorType = "";
    if (Objects.nonNull(schema.getAllOf())) {
      schemaCombinatorType = ALL_OF_COMBINATOR;
    }

    if (Objects.nonNull(schema.getAnyOf())) {
      schemaCombinatorType = ANY_OF_COMBINATOR;
    }

    if (Objects.nonNull(schema.getOneOf())) {
      schemaCombinatorType = ONE_OF_COMBINATOR;
    }

    compositedSchemas.put(StringUtils.defaultIfBlank(schema.getName(), nameSchema), SchemaObject.builder()
                                                                                                .schemaName(StringUtils.defaultIfBlank(schema.getName(), nameSchema))
                                                                                                .className(MapperUtil.getPojoName(nameSchema, specFile))
                                                                                                .importList(getImportList(listSchema, modelPackage))
                                                                                                .schemaCombinator(schemaCombinatorType)
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
        listHashMap.put(StringUtils.capitalize(fieldObject.getImportClass()),
                        List.of(modelPackage + "." + StringUtils.capitalize(fieldObject.getImportClass())));
      }
    }
    if (!listHashMap.isEmpty()) {
      listHashMap.forEach((key, value) -> importList.addAll(value));
    }
    return importList;
  }

  private static void getTypeImports(final HashMap<String, List<String>> listHashMap, final SchemaFieldObject fieldObject) {
    final SchemaFieldObjectType type = fieldObject.getDataType();
    if (type.containsType(TypeConstants.ARRAY)) {
      listHashMap.computeIfAbsent(TypeConstants.ARRAY, key -> List.of("java.util.List", "java.util.ArrayList"));
    }

    if (type.containsType(TypeConstants.MAP)) {
      listHashMap.computeIfAbsent(TypeConstants.MAP, key -> List.of("java.util.Map", "java.util.HashMap"));
    }

    if (type.containsType(TypeConstants.BIG_DECIMAL)) {
      listHashMap.computeIfAbsent(TypeConstants.BIG_DECIMAL, key -> List.of("java.math.BigDecimal"));
    }

    if (type.containsType(TypeConstants.LOCALDATE)) {
      listHashMap.computeIfAbsent(TypeConstants.LOCALDATE, key -> List.of("java.time.LocalDate"));
    }

    if (type.containsType(TypeConstants.LOCALDATETIME)) {
      listHashMap.computeIfAbsent(TypeConstants.LOCALDATETIME, key -> List.of("java.time.LocalDateTime"));
    }

    if (type.containsType(TypeConstants.ZONEDDATE)) {
      listHashMap.computeIfAbsent(TypeConstants.ZONEDDATETIME, key -> List.of("java.time.ZonedDateTime"));
    }

    if (type.containsType(TypeConstants.ZONEDDATETIME)) {
      listHashMap.computeIfAbsent(TypeConstants.ZONEDDATETIME, key -> List.of("java.time.ZonedDateTime"));
    }

    if (type.containsType(TypeConstants.OFFSETDATE)) {
      listHashMap.computeIfAbsent(TypeConstants.OFFSETDATETIME, key -> List.of("java.time.OffsetDateTime"));
    }

    if (type.containsType(TypeConstants.OFFSETDATETIME)) {
      listHashMap.computeIfAbsent(TypeConstants.OFFSETDATETIME, key -> List.of("java.time.OffsetDateTime"));
    }
  }

  private static List<SchemaFieldObject> getFields(
      final Map<String, Schema> totalSchemas, final Schema<?> schema, final SpecFile specFile, final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (Objects.nonNull(schema.getProperties())) {
      if (Objects.nonNull(schema.getAdditionalProperties())) {
        schema.getProperties().forEach(
            processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
        fieldObjectArrayList.addAll(
            processFieldObjectList("additionalProperties", null, schema, specFile, totalSchemas, compositedSchemas,
                                   antiLoopList));
      } else {
        fieldObjectArrayList.addAll(
            processFieldObjectList(null, null, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
      }
    } else if (TypeConstants.ARRAY.equalsIgnoreCase(schema.getType())) {
      final ArraySchema arraySchema = (ArraySchema) schema;
      final String itemType = Objects.nonNull(arraySchema.getItems().get$ref()) ? getRef(arraySchema.getItems(), specFile) : arraySchema.getItems().getType();

      final var field = SchemaFieldObject.builder()
                                         .baseName("items")
                                         .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, itemType))
                                         .build();
      fieldObjectArrayList.add(field);
    } else if (Objects.nonNull(schema.getAllOf())) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, schema.getAllOf(), specFile, compositedSchemas, antiLoopList));
    } else if (Objects.nonNull(schema.getAnyOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getAnyOf(), specFile, compositedSchemas, antiLoopList));
    } else if (Objects.nonNull(schema.getOneOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getOneOf(), specFile, compositedSchemas, antiLoopList));
    } else if (Objects.nonNull(schema.getEnum())) {
      processEnumField(schema.getName(), schema, specFile, fieldObjectArrayList, schema.getEnum(), schema);
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAllOf(
      final Map<String, Schema> totalSchemas, final List<Schema> schemaList, final SpecFile specFile, final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (Schema<?> ref : schemaList) {
      if (Objects.nonNull(ref.get$ref())) {
        final String[] pathObjectRef = ref.get$ref().split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        fieldObjectArrayList.addAll(
            processFieldObjectList(null, null, schemaToProcess, specFile, totalSchemas, compositedSchemas,
                                   antiLoopList));
        for (var fieldObject : fieldObjectArrayList) {
          fieldObject.setRequired(true);
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAnyOfOneOf(
      final Map<String, Schema> totalSchemas, final List<Schema> schemaList, final SpecFile specFile, final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (Schema<?> internalSchema : schemaList) {
      if (Objects.nonNull(internalSchema.get$ref())) {
        final String[] pathObjectRef = internalSchema.get$ref().split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        if (compositedSchemas.containsKey(schemaName)) {
          fieldObjectArrayList.add(SchemaFieldObject
                                       .builder()
                                       .baseName(schemaName)
                                       .dataType(new SchemaFieldObjectType(schemaName))
                                       .build());
        } else {
          fieldObjectArrayList.addAll(
              processFieldObjectList(null, schemaName, schemaToProcess, specFile, totalSchemas, compositedSchemas,
                                     antiLoopList));
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
      final String fieldName, final String className, final Schema<?> schema, final SpecFile specFile, final Map<String, Schema> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new LinkedList<SchemaFieldObject>();

    if (TypeConstants.ARRAY.equalsIgnoreCase(schema.getType())) {
      fieldObjectArrayList.addAll(processArray(fieldName, className, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
    } else if (schema instanceof MapSchema) {
      fieldObjectArrayList.addAll(processMap(fieldName, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
    } else if (Objects.nonNull(schema.get$ref())) {
      final String refSchemaName = getRef(schema, specFile);
      final var field = SchemaFieldObject.builder()
                                         .baseName(fieldName)
                                         .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(schema, specFile)))
                                         .build();
      setFieldType(field, schema, schema, specFile, refSchemaName);
      fieldObjectArrayList.add(field);
    } else if (!(schema instanceof ObjectSchema || schema instanceof ComposedSchema)) {
      final var field = SchemaFieldObject
                            .builder()
                            .baseName(fieldName)
                            .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(schema, specFile)))
                            .build();
      setFieldType(field, schema, schema, specFile, "");
      fieldObjectArrayList.add(field);
    } else if (MapUtils.isEmpty(schema.getProperties()) && !(schema instanceof ComposedSchema)) {
      fieldObjectArrayList.add(SchemaFieldObject
                                   .builder()
                                   .baseName(fieldName)
                                   .dataType(new SchemaFieldObjectType(TypeConstants.OBJECT))
                                   .build());
    } else if (schema instanceof ObjectSchema) {
      fieldObjectArrayList.addAll(processObject(fieldName, className, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
    } else {
      final var composedSchemaName = StringUtils.defaultIfBlank(className, fieldName);
      var schemaObjectComposed = compositedSchemas.get(composedSchemaName);
      if (Objects.isNull(schemaObjectComposed)) {
        schemaObjectComposed = createComposedSchema(StringUtils.defaultIfBlank(className, fieldName), schema, specFile,
                                                    totalSchemas, compositedSchemas, antiLoopList);
        compositedSchemas.put(composedSchemaName, schemaObjectComposed);
      }

      fieldObjectArrayList.add(SchemaFieldObject
                                   .builder()
                                   .baseName(fieldName)
                                   .dataType(SchemaFieldObjectType.fromTypeList(schemaObjectComposed.getClassName(), schemaObjectComposed.getClassName()))
                                   .build());
    }

    return fieldObjectArrayList;
  }

  private static BiConsumer<String, Schema> processProperties(
      final Map<String, Schema> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final List<SchemaFieldObject> fieldObjectArrayList, final SpecFile specFile,
      final Schema<?> schema, final List<String> antiLoopList) {
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
      final Map<String, Schema> totalSchemas, final String key, final Schema value, final Map<String, SchemaObject> compositedSchemas, final SpecFile specFile,
      final Schema<?> schema, final List<String> antiLoopList) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();

    final SchemaFieldObject field;
    if (Objects.nonNull(value.get$ref())) {
      final var typeName = cleanRefName(value);

      if (!antiLoopList.contains(typeName) && totalSchemas.containsKey(typeName) && Objects.nonNull(totalSchemas.get(typeName).getType())
          && totalSchemas.get(typeName).getType().equalsIgnoreCase(TypeConstants.ARRAY) || value.get$ref().contains(key)) {
        antiLoopList.add(typeName);
        fieldObjectArrayList.addAll(processFieldObjectList(key, typeName, totalSchemas.get(typeName), specFile, totalSchemas, compositedSchemas, antiLoopList));
      } else {
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(key)
                                     .dataType(SchemaFieldObjectType.fromTypeList(MapperUtil.getSimpleType(totalSchemas.get(typeName), specFile),
                                                                                  MapperUtil.getPojoName(typeName, specFile)))
                                     .build());
      }
    } else if (TypeConstants.STRING.equalsIgnoreCase(value.getType())) {
      field = processStringProperty(key, value, specFile);
      setFieldType(field, value, schema, specFile, key);
      fieldObjectArrayList.add(field);
    } else if (isBasicType(value)) {
      field = SchemaFieldObject
                  .builder()
                  .baseName(key)
                  .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(value, specFile)))
                  .build();
      addPropertiesToFieldObject(field, value);
      setFieldType(field, value, schema, specFile, key);
      fieldObjectArrayList.add(field);
    } else {
      fieldObjectArrayList.addAll(processFieldObjectList(key, key, value, specFile, totalSchemas, compositedSchemas, antiLoopList));
    }
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject processStringProperty(final String propertyName, final Schema<?> schema, final SpecFile specFile) {
    final String resultingType = schema instanceof DateSchema ? getDateType(specFile) : (schema instanceof DateTimeSchema ? getDateTimeType(specFile) : TypeConstants.STRING);
    final SchemaFieldObject field = SchemaFieldObject
                                        .builder()
                                        .baseName(propertyName)
                                        .dataType(new SchemaFieldObjectType(resultingType))
                                        .build();
    addPropertiesToFieldObject(field, schema);
    return field;
  }

  private static void addPropertiesToFieldObject(final SchemaFieldObject fieldObject, final Schema value) {
    fieldObject.getRestrictionProperties().setPattern(value.getPattern());
    fieldObject.getRestrictionProperties().setMaxItems(value.getMaxItems());
    fieldObject.getRestrictionProperties().setMinItems(value.getMinItems());
    fieldObject.getRestrictionProperties().setMaxLength(value.getMaxLength());
    fieldObject.getRestrictionProperties().setMinLength(value.getMinLength());
    fieldObject.getRestrictionProperties().setUniqueItems(value.getUniqueItems());
    fieldObject.getRestrictionProperties().setExclusiveMaximum(value.getExclusiveMaximum());
    fieldObject.getRestrictionProperties().setExclusiveMinimum(value.getExclusiveMinimum());
    if (Objects.nonNull(value.getMultipleOf())) {
      fieldObject.getRestrictionProperties().setMultipleOf(value.getMultipleOf().toString());
    }
    if (Objects.nonNull(value.getMaximum())) {
      fieldObject.getRestrictionProperties().setMaximum(value.getMaximum().toString());
    }
    if (Objects.nonNull(value.getMinimum())) {
      fieldObject.getRestrictionProperties().setMinimum(value.getMinimum().toString());
    }
  }

  private static List<SchemaFieldObject> processArray(
      final String fieldName, final String className, final Schema<?> schema, final SpecFile specFile, final Map<String, Schema> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();

    if (Objects.isNull(schema.getItems())) {
      fieldObjectArrayList.add(SchemaFieldObject
                                   .builder()
                                   .baseName(fieldName)
                                   .dataType(new SchemaFieldObjectType(TypeConstants.OBJECT))
                                   .build());
    } else {
      final var items = schema.getItems();
      if (Objects.nonNull(items.get$ref())) {
        final String refSchemaName = getRef(items, specFile);
        final var field = SchemaFieldObject
                              .builder()
                              .baseName(fieldName)
                              .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, MapperUtil.getSimpleType(items, specFile)))
                              .build();
        setFieldType(field, items, items, specFile, refSchemaName);
        fieldObjectArrayList.add(field);
      } else if (ObjectUtils.anyNotNull(items.getAnyOf(), items.getAllOf(), items.getOneOf())) {
        final String composedSchemaName = StringUtils.defaultIfBlank(className, fieldName);
        SchemaObject schemaObjectComposed = compositedSchemas.get(composedSchemaName);
        if (Objects.isNull(schemaObjectComposed)) {
          schemaObjectComposed = createComposedSchema(StringUtils.defaultIfBlank(className, fieldName), items, specFile,
                                                      totalSchemas, compositedSchemas, antiLoopList);
          compositedSchemas.put(composedSchemaName, schemaObjectComposed);
        }

        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, schemaObjectComposed.getClassName()))
                                     .importClass(schemaObjectComposed.getClassName())
                                     .build());
      } else if (Objects.nonNull(items.getProperties())) {
        compositedSchemas.putAll(mapComponentToSchemaObject(totalSchemas, items, fieldName, specFile));
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, MapperUtil.getPojoName(fieldName, specFile)))
                                     .build());
      } else {
        final SchemaFieldObject field = SchemaFieldObject
                                            .builder()
                                            .baseName(fieldName)
                                            .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, MapperUtil.getSimpleType(items, specFile)))
                                            .build();
        fieldObjectArrayList.add(field);
        addPropertiesToFieldObject(field, schema);
      }
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processObject(
      final String fieldName, final String className, final Schema<?> schema, final SpecFile specFile, final Map<String, Schema> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();

    if (ObjectUtils.allNull(className, fieldName)) {
      schema.getProperties().forEach(
          processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
    } else if (antiLoopList.contains(className)) {
      fieldObjectArrayList
          .add(SchemaFieldObject
                   .builder()
                   .baseName(className)
                   .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, MapperUtil.getPojoName(className, specFile)))
                   .build());
    } else if (antiLoopList.contains(fieldName)) {
      fieldObjectArrayList
          .add(SchemaFieldObject
                   .builder()
                   .baseName(fieldName)
                   .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, MapperUtil.getPojoName(fieldName, specFile)))
                   .build());
    } else {
      final String name = StringUtils.defaultIfBlank(className, fieldName);
      compositedSchemas.putAll(
          mapComponentToSchemaObject(totalSchemas, compositedSchemas, antiLoopList, schema, name, specFile,
                                     specFile.getModelPackage()));
      fieldObjectArrayList
          .add(SchemaFieldObject
                   .builder()
                   .baseName(name)
                   .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, MapperUtil.getPojoName(name, specFile)))
                   .build());
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processMap(
      final String fieldName, final Schema<?> schema, final SpecFile specFile, final Map<String, Schema> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (TypeConstants.OBJECT.equalsIgnoreCase(schema.getType())) {
      schema.getProperties().forEach(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
    }

    if (Objects.nonNull(schema.getAdditionalProperties())) {
      fieldObjectArrayList.addAll(processAdditionalProperties(fieldName, schema, specFile));
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAdditionalProperties(final String fieldName, final Schema<?> schema, final SpecFile specFile) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    final Object addPropObj = schema.getAdditionalProperties();
    if (addPropObj instanceof Boolean) {
      fieldObjectArrayList
          .add(SchemaFieldObject
                   .builder()
                   .baseName(fieldName)
                   .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.OBJECT))
                   .build());
    } else {
      final Schema<?> additionalProperties = (Schema<?>) schema.getAdditionalProperties();
      if (Objects.nonNull(additionalProperties.get$ref())) {
        final String refSchemaName = getRef(additionalProperties, specFile);
        final var field = SchemaFieldObject.builder()
                                           .baseName(fieldName)
                                           .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, refSchemaName))
                                           .build();
        setFieldType(field, schema, additionalProperties, specFile, refSchemaName);
        fieldObjectArrayList.add(field);
      } else if (additionalProperties instanceof ArraySchema) {
        fieldObjectArrayList
            .add(SchemaFieldObject
                     .builder()
                     .baseName("additionalProperties")
                     .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.ARRAY, MapperUtil.getSimpleType(additionalProperties.getItems(), specFile)))
                     .build());
      } else {
        final String type = isBasicType(additionalProperties) ? MapperUtil.getSimpleType(additionalProperties, specFile)
                                : MapperUtil.getPojoName(schema.getName() + ADDITIONAL_PROPERTY_NAME, specFile);
        fieldObjectArrayList
            .add(SchemaFieldObject
                     .builder()
                     .baseName(fieldName)
                     .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, type))
                     .build());
      }
    }

    return fieldObjectArrayList;
  }

  private static boolean isBasicType(final Schema value) {
    return !(value instanceof ArraySchema || value instanceof ObjectSchema || value instanceof ComposedSchema || value instanceof MapSchema);
  }

  private static SchemaObject createComposedSchema(
      final String fieldName, final Schema<?> schema, final SpecFile specFile, final Map<String, Schema> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    String schemaCombinatorType = "";
    if (Objects.nonNull(schema.getAllOf())) {
      fieldObjectArrayList.addAll(
          processAllOf(totalSchemas, schema.getAllOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = ALL_OF_COMBINATOR;
    } else if (Objects.nonNull(schema.getAnyOf())) {
      fieldObjectArrayList.addAll(
          processAnyOfOneOf(totalSchemas, schema.getAnyOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = ANY_OF_COMBINATOR;
    } else if (Objects.nonNull(schema.getOneOf())) {
      fieldObjectArrayList.addAll(
          processAnyOfOneOf(totalSchemas, schema.getOneOf(), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = ONE_OF_COMBINATOR;
    }

    return SchemaObject.builder()
                       .schemaName(fieldName)
                       .className(MapperUtil.getPojoName(fieldName, specFile))
                       .importList(getImportList(fieldObjectArrayList, specFile.getModelPackage()))
                       .schemaCombinator(schemaCombinatorType)
                       .fieldObjectList(fieldObjectArrayList)
                       .build();
  }

  private static void setFieldType(final SchemaFieldObject field, final Schema<?> value, final Schema<?> schema, final SpecFile specFile, final String key) {
    field.setRequired(Objects.nonNull(schema.getRequired()) && schema.getRequired().contains(key));
    if (Objects.nonNull(value.getType()) && TypeConstants.ARRAY.equalsIgnoreCase(value.getType())) {
      final String typeArray;
      if (Objects.nonNull(value.getItems())) {
        typeArray = MapperUtil.getTypeArray((ArraySchema) value, specFile);
      } else {
        typeArray = TypeConstants.OBJECT;
      }
      field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, typeArray));
      field.setImportClass(getImportClass(typeArray));
    } else if (value instanceof MapSchema) {
      if (Objects.nonNull(value.getAdditionalProperties())) {
        final String typeObject = getMapTypeObject(value, specFile);
        field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, typeObject));
        field.setImportClass(getImportClass(typeObject));
      } else {
        final var typeMap = MapperUtil.getTypeMap((MapSchema) value, specFile);
        field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, typeMap));
        field.setImportClass(getImportClass(typeMap));
      }
    } else if (Objects.nonNull(value.getType()) && TypeConstants.OBJECT.equalsIgnoreCase(value.getType())) {
      var typeObject = "";
      if (StringUtils.isNotBlank(value.get$ref())) {
        typeObject = getRef(schema, specFile);
      }
      field.setImportClass(getImportClass(typeObject));
      field.getDataType().setDeepType(typeObject);
    }
  }

  private static String getMapTypeObject(final Schema schema, final SpecFile specFile) {
    final String type;
    if (schema.getAdditionalProperties() instanceof Boolean) {
      type = TypeConstants.OBJECT;
    } else {
      final Schema<?> additionalProperties = (Schema<?>) schema.getAdditionalProperties();
      if (StringUtils.isNotBlank(additionalProperties.get$ref())) {
        type = getRef(additionalProperties, specFile);
      } else if (StringUtils.isNotBlank(additionalProperties.getType()) && !additionalProperties.getType().equalsIgnoreCase(TypeConstants.OBJECT)) {
        final var additionalPropertiesField = SchemaFieldObject
                                                  .builder()
                                                  .baseName(additionalProperties.getName())
                                                  .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(additionalProperties, specFile)))
                                                  .build();
        setFieldType(additionalPropertiesField, additionalProperties, additionalProperties, specFile, "");
        type = getMapFieldType(additionalPropertiesField);
      } else {
        type = TypeConstants.OBJECT;
      }
    }

    return type;
  }

  private static String getMapFieldType(final SchemaFieldObject schemaFieldObject) {
    final String fieldType = schemaFieldObject.getDataType().toString();
    final String type;
    switch (fieldType) {
      case TypeConstants.BIG_DECIMAL:
      case TypeConstants.INTEGER:
      case TypeConstants.DOUBLE:
      case TypeConstants.FLOAT:
      case TypeConstants.LONG:
      case TypeConstants.STRING:
        type = fieldType;
        break;
      default:
        type = TypeConstants.OBJECT;
        break;
    }

    return type;
  }

  public static String getRef(final Schema<?> schema, final SpecFile specFile) {
    final String typeObject;
    typeObject = MapperUtil.getPojoName(cleanRefName(schema), specFile);
    return typeObject;
  }

  public static String cleanRefName(final Schema<?> schema) {
    final String[] pathObjectRef = schema.get$ref().split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  private static void processEnumField(
      final String key, final Schema<?> value, final SpecFile specFile, final List<SchemaFieldObject> fieldObjectArrayList, final List<?> enumValues, final Schema<?> schema) {
    final var field = SchemaFieldObject
                          .builder()
                          .baseName(key)
                          .dataType(new SchemaFieldObjectType(TypeConstants.ENUM))
                          .build();
    field.setRequired(Objects.nonNull(schema.getRequired()) && schema.getRequired().contains(key));
    final var dataType = MapperUtil.getSimpleType(value, specFile);
    field.getDataType().setDeepType(dataType);

    final HashMap<String, String> enumValuesMap = new HashMap<>();

    for (var enumValue : enumValues) {
      String valueName = enumValue.toString();
      valueName = valueName.replace(".", "_DOT_");

      switch (dataType) {
        case TypeConstants.INTEGER:
          enumValuesMap.put("INTEGER_" + valueName, enumValue.toString());
          break;
        case TypeConstants.LONG:
          enumValuesMap.put("LONG_" + valueName, enumValue + "l");
          break;
        case TypeConstants.DOUBLE:
          enumValuesMap.put("DOUBLE_" + valueName, enumValue.toString());
          break;
        case TypeConstants.FLOAT:
          enumValuesMap.put("FLOAT_" + valueName, enumValue + "f");
          break;
        case TypeConstants.BIG_DECIMAL:
          enumValuesMap.put("BIG_DECIMAL_" + valueName, "new BigDecimal(\"" + enumValue + "\")");
          break;
        case TypeConstants.STRING:
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
    return StringUtils.isNotBlank(type) && !TypeConstants.NO_IMPORT_TYPE.contains(type) ? StringUtils.capitalize(type) : "";
  }

  public static String getDateType(final SpecFile specFile) {
    final String dateType;
    switch (specFile.getUseTimeType()) {
      case ZONED:
        dateType = TypeConstants.ZONEDDATE;
        break;
      case OFFSET:
        dateType = TypeConstants.OFFSETDATE;
        break;
      default:
        dateType = TypeConstants.LOCALDATE;
    }

    return dateType;
  }

  private static String getDateTimeType(final SpecFile specFile) {
    final String dateTimeType;
    switch (specFile.getUseTimeType()) {
      case ZONED:
        dateTimeType = TypeConstants.ZONEDDATETIME;
        break;
      case OFFSET:
        dateTimeType = TypeConstants.OFFSETDATETIME;
        break;
      default:
        dateTimeType = TypeConstants.LOCALDATETIME;
    }

    return dateTimeType;
  }

}
