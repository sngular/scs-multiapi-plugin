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
import java.util.function.Consumer;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.openapi.model.SchemaObject;
import com.sngular.api.generator.plugin.openapi.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {

  private static final String ADDITIONAL_PROPERTY_NAME = "AdditionalProperty";

  private static final String ANY_OF_COMBINATOR = "anyOf";

  private static final String ONE_OF_COMBINATOR = "oneOf";

  private static final String ALL_OF_COMBINATOR = "allOf";
  public static final String ADDITIONAL_PROPERTIES = "additionalProperties";

  private MapperContentUtil() {
  }

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas, final JsonNode schema, final String schemaName,
      final SpecFile specFile) {
    return mapComponentToSchemaObject(totalSchemas, new HashMap<>(), new ArrayList<>(), schema,
            StringUtils.defaultIfEmpty(ApiTool.getNodeAsString(schema, "name"), schemaName), specFile, specFile.getModelPackage());
  }

  private static Map<String, SchemaObject> mapComponentToSchemaObject(
          final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
          final List<String> antiLoopList, final JsonNode schema, final String schemaName, final SpecFile specFile,
          final String modelPackage) {
    antiLoopList.add(schemaName);

    final var listSchema = getFields(totalSchemas, schema, specFile, compositedSchemas, antiLoopList, schemaName);

    String schemaCombinatorType = "";
    if (ApiTool.isAllOf(schema)) {
      schemaCombinatorType = ALL_OF_COMBINATOR;
    }

    if (ApiTool.isAnyOf(schema)) {
      schemaCombinatorType = ANY_OF_COMBINATOR;
    }

    if (ApiTool.isOneOf(schema)) {
      schemaCombinatorType = ONE_OF_COMBINATOR;
    }

    final var name = StringUtils.defaultIfBlank(ApiTool.getNodeAsString(schema, "name"), schemaName);
    compositedSchemas
            .put(name,
                    SchemaObject.builder()
                        .schemaName(name)
                        .className(MapperUtil.getPojoName(name, specFile))
                        .importList(getImportList(listSchema, modelPackage))
                        .schemaCombinator(schemaCombinatorType)
                        .fieldObjectList(listSchema)
                        .build());
    return compositedSchemas;
  }

  private static List<String> getImportList(final List<SchemaFieldObject> fieldObjectList, final String modelPackage) {
    final var listHashMap = new HashMap<String, List<String>>();
    final var importList = new ArrayList<String>();

    for (final var fieldObject : fieldObjectList) {
      getTypeImports(listHashMap, fieldObject);
      if (StringUtils.isNotBlank(fieldObject.getImportClass())
              && !listHashMap.containsKey(fieldObject.getImportClass())
              && !fieldObject.getDataType().containsType("enum")) {
        listHashMap.put(StringUtils.capitalize(fieldObject.getImportClass()),
                        List.of(modelPackage + "." + StringUtils.capitalize(fieldObject.getImportClass())));
      }
    }
    if (!listHashMap.isEmpty()) {
      listHashMap.forEach((key, value) -> importList.addAll(value));
    }
    return importList;
  }

  private static void getTypeImports(final HashMap<String, List<String>> listHashMap,
                                     final SchemaFieldObject fieldObject) {
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
          final Map<String, JsonNode> totalSchemas, final JsonNode schema, final SpecFile specFile,
          final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final String nameSchema) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (ApiTool.hasProperties(schema)) {
      if (ApiTool.hasAdditionalProperties(schema)) {
        ApiTool
                .getProperties(schema)
                .forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
        fieldObjectArrayList.addAll(
                processAdditionalProperties(ADDITIONAL_PROPERTIES, schema, specFile, totalSchemas, compositedSchemas, antiLoopList, nameSchema));
      } else {
        ApiTool.getProperties(schema).forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
      }
    } else if (TypeConstants.ARRAY.equalsIgnoreCase(ApiTool.getType(schema))) {
      final String itemType = ApiTool.hasRef(ApiTool.getItems(schema)) ? getRef(ApiTool.getItems(schema), specFile) : ApiTool.getType(ApiTool.getItems(schema));
      fieldObjectArrayList.add(SchemaFieldObject.builder()
                                         .baseName("items")
                                         .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, itemType))
                                         .build());
    } else if (ApiTool.isAllOf(schema)) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, ApiTool.getAllOf(schema), specFile, compositedSchemas, antiLoopList));
    } else if (ApiTool.isAnyOf(schema)) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, ApiTool.getAnyOf(schema), specFile, compositedSchemas, antiLoopList));
    } else if (ApiTool.isOneOf(schema)) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, ApiTool.getOneOf(schema), specFile, compositedSchemas, antiLoopList));
    } else if (ApiTool.isEnum(schema)) {
      fieldObjectArrayList.add(processEnumField(ApiTool.getName(schema), schema, specFile, ApiTool.getEnumValues(schema), schema));
    } else if (ApiTool.hasRef(schema)) {
      final var refSchema = totalSchemas.get(cleanRefName(ApiTool.getRefValue(schema)));
      ApiTool.getProperties(refSchema).forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
    } else {
      fieldObjectArrayList.add(SchemaFieldObject.builder()
                      .baseName(ApiTool.getName(schema))
                      .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(schema, specFile)))
                      .build());
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAllOf(
      final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final SpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (JsonNode ref : schemaList) {
      if (ApiTool.hasRef(ref)) {
        final String[] pathObjectRef = ApiTool.getRefValue(ref).split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        ApiTool.getProperties(schemaToProcess).forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, ref, antiLoopList));
        for (var fieldObject : fieldObjectArrayList) {
          fieldObject.setRequired(true);
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAnyOfOneOf(
      final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final SpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (JsonNode internalSchema : schemaList) {
      if (ApiTool.hasRef(internalSchema)) {
        final String[] pathObjectRef = ApiTool.getRefValue(internalSchema).split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        if (compositedSchemas.containsKey(schemaName) || antiLoopList.contains(schemaName)) {
          fieldObjectArrayList.add(SchemaFieldObject
                                       .builder()
                                       .baseName(schemaName)
                                       .dataType(SchemaFieldObjectType.fromTypeList(MapperUtil.getSimpleType(totalSchemas.get(schemaName), specFile),
                                               MapperUtil.getPojoName(schemaName, specFile)))
                                       .build());
        } else {
          antiLoopList.add(schemaName);
          ApiTool.getProperties(schemaToProcess)
                  .forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schemaToProcess, antiLoopList));
        }
      } else {
        for (var fieldObject : fieldObjectArrayList) {
          if (ApiTool.checkIfRequired(internalSchema, fieldObject.getBaseName())) {
            fieldObject.setRequired(true);
          }
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processFieldObjectList(
      final String fieldName, final String className, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (TypeConstants.ARRAY.equalsIgnoreCase(ApiTool.getType(schema))) {
      fieldObjectArrayList.addAll(processArray(fieldName, className, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
    } else if (ApiTool.hasAdditionalProperties(schema)) {
      fieldObjectArrayList.addAll(processMap(fieldName, schema, specFile, totalSchemas, compositedSchemas, antiLoopList));
    } else if (ApiTool.hasRef(schema)) {
      fieldObjectArrayList.add(processRef(fieldName, schema, specFile));
    } else if (ApiTool.isObject(schema) && !ApiTool.hasProperties(schema)) {
      fieldObjectArrayList.add(SchemaFieldObject
              .builder()
              .baseName(fieldName)
              .dataType(new SchemaFieldObjectType(TypeConstants.OBJECT))
              .build());
    } else if (!ApiTool.hasProperties(schema) && !ApiTool.isComposed(schema)) {
      fieldObjectArrayList.add(SchemaFieldObject
                                   .builder()
                                   .baseName(fieldName)
                                   .dataType(new SchemaFieldObjectType(TypeConstants.OBJECT))
                                   .build());
    } else if (ApiTool.isObject(schema)) {
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

  private static SchemaFieldObject processRef(final String fieldName, final JsonNode schema, final SpecFile specFile) {
    final String refSchemaName = getRef(schema, specFile);
    final var field = SchemaFieldObject.builder()
                                       .baseName(fieldName)
                                       .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(schema, specFile)))
                                       .build();
    setFieldType(field, schema, schema, specFile, refSchemaName);
    return field;
  }

  private static Consumer<Map.Entry<String, JsonNode>> processProperties(
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final List<SchemaFieldObject> fieldObjectArrayList, final SpecFile specFile,
      final JsonNode schema, final List<String> antiLoopList) {
    return field -> {
      final var nodeName = field.getKey();
      final var nodeValue = field.getValue();
      if (ApiTool.isEnum(field.getValue())) {
        fieldObjectArrayList.add(processEnumField(nodeName, nodeValue, specFile, ApiTool.getEnumValues(nodeValue), schema));
      } else {
        fieldObjectArrayList.addAll(processObjectProperty(totalSchemas, nodeName, nodeValue, compositedSchemas, specFile, schema, antiLoopList));
      }
    };
  }

  private static List<SchemaFieldObject> processObjectProperty(
      final Map<String, JsonNode> totalSchemas, final String key, final JsonNode value, final Map<String, SchemaObject> compositedSchemas, final SpecFile specFile,
      final JsonNode schema, final List<String> antiLoopList) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();
    final var isRequired = ApiTool.checkIfRequired(value, key);
    final SchemaFieldObject field;
    if (ApiTool.hasRef(value)) {
      final var typeName = cleanRefName(ApiTool.getRefValue(value));

      if (!antiLoopList.contains(typeName) && totalSchemas.containsKey(typeName) && ApiTool.hasType(totalSchemas.get(typeName))
          && ApiTool.hasItems(totalSchemas.get(typeName)) || ApiTool.getRefValue(value).contains(key)) {
        antiLoopList.add(typeName);
        fieldObjectArrayList.addAll(processFieldObjectList(key, typeName, totalSchemas.get(typeName), specFile, totalSchemas, compositedSchemas, antiLoopList));
      } else {
        fieldObjectArrayList.add(SchemaFieldObject
                 .builder()
                 .baseName(key)
                 .required(isRequired)
                 .dataType(SchemaFieldObjectType.fromTypeList(MapperUtil.getSimpleType(totalSchemas.get(typeName), specFile),
                                                              MapperUtil.getPojoName(typeName, specFile)))
                 .build());
      }
    } else if (TypeConstants.STRING.equalsIgnoreCase(ApiTool.getType(value))) {
      field = processStringProperty(key, value, specFile);
      setFieldType(field, value, schema, specFile, key);
      fieldObjectArrayList.add(field);
    } else if (isBasicType(value)) {
      field = SchemaFieldObject
                  .builder()
                  .baseName(key)
                  .required(isRequired)
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

  private static SchemaFieldObject processStringProperty(final String propertyName, final JsonNode schema, final SpecFile specFile) {
    final String resultingType = ApiTool.isDateTime(schema) ? getDateType(schema, specFile) : TypeConstants.STRING;

    final SchemaFieldObject field = SchemaFieldObject
                                        .builder()
                                        .baseName(propertyName)
                                        .required(ApiTool.checkIfRequired(schema, propertyName))
                                        .dataType(new SchemaFieldObjectType(resultingType))
                                        .build();
    addPropertiesToFieldObject(field, schema);
    return field;
  }

  private static void addPropertiesToFieldObject(final SchemaFieldObject fieldObject, final JsonNode value) {
    final var restrictionList = ApiTool.getFieldIterator(value);
    while (restrictionList.hasNext()) {
      final var restriction = restrictionList.next();
      switch (restriction.getKey()) {
        case "pattern":
          fieldObject.getRestrictionProperties().setPattern(restriction.getValue().asText());
          break;
        case "maxItems":
          fieldObject.getRestrictionProperties().setMaxItems(restriction.getValue().asInt());
          break;
        case "minItems":
          fieldObject.getRestrictionProperties().setMinItems(restriction.getValue().asInt());
          break;
        case "maxLength":
          fieldObject.getRestrictionProperties().setMaxLength(restriction.getValue().asInt());
          break;
        case "minLength":
          fieldObject.getRestrictionProperties().setMinLength(restriction.getValue().asInt());
          break;
        case "uniqueItems":
          fieldObject.getRestrictionProperties().setUniqueItems(restriction.getValue().asBoolean());
          break;
        case "exclusiveMaximum":
          fieldObject.getRestrictionProperties().setExclusiveMaximum(restriction.getValue().asBoolean());
          break;
        case "exclusiveMinimum":
          fieldObject.getRestrictionProperties().setExclusiveMinimum(restriction.getValue().asBoolean());
          break;
        case "multipleOf":
          fieldObject.getRestrictionProperties().setMultipleOf(restriction.getValue().asText());
          break;
        case "maximum":
          fieldObject.getRestrictionProperties().setMaximum(restriction.getValue().asText());
          break;
        case "minimum":
          fieldObject.getRestrictionProperties().setMinimum(restriction.getValue().asText());
          break;
        default:
          break;
      }
    }
  }

  private static List<SchemaFieldObject> processArray(
      final String fieldName, final String className, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();

    if (!ApiTool.hasItems(schema)) {
      fieldObjectArrayList.add(SchemaFieldObject
                                   .builder()
                                   .baseName(fieldName)
                                   .dataType(new SchemaFieldObjectType(TypeConstants.OBJECT))
                                   .build());
    } else {
      final var items = ApiTool.getItems(schema);
      if (ApiTool.hasRef(items)) {
        final String refSchemaName = getRef(items, specFile);
        final var field = SchemaFieldObject
                              .builder()
                              .baseName(fieldName)
                              .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, MapperUtil.getSimpleType(items, specFile)))
                              .build();
        setFieldType(field, items, items, specFile, refSchemaName);
        fieldObjectArrayList.add(field);
        final var refSchema = totalSchemas.get(cleanRefName(items));
        if (ApiTool.isEnum(refSchema)) {
          fieldObjectArrayList.add(processEnumField(refSchemaName, refSchema, specFile, ApiTool.getEnumValues(refSchema), schema));
        }
      } else if (ApiTool.isComposed(items)) {
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
      } else if (ApiTool.hasProperties(items)) {
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
      final String fieldName, final String className, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();

    if (ObjectUtils.allNull(className, fieldName)) {
      ApiTool.getProperties(schema).forEachRemaining(
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
      final String fieldName, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (TypeConstants.OBJECT.equalsIgnoreCase(ApiTool.getType(schema))) {
      ApiTool.getProperties(schema).forEachRemaining(
          processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList));
    }

    if (ApiTool.hasProperties(schema)) {
      fieldObjectArrayList.addAll(processAdditionalProperties(fieldName, schema, specFile, totalSchemas, compositedSchemas,
              antiLoopList, ADDITIONAL_PROPERTIES));
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAdditionalProperties(
          final String fieldName, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
          final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final String nameSchema) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    final var addPropObj = ApiTool.getAdditionalProperties(schema);
    if (TypeConstants.isBoolean(addPropObj.asText())) {
      fieldObjectArrayList
          .add(SchemaFieldObject
                   .builder()
                   .baseName(fieldName)
                   .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.OBJECT))
                   .build());
    } else if (ApiTool.hasRef(addPropObj)) {
      final String refSchemaName = getRef(addPropObj, specFile);
      final var field = SchemaFieldObject.builder()
                                         .baseName(fieldName)
                                         .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, refSchemaName))
                                         .build();
      setFieldType(field, schema, addPropObj, specFile, refSchemaName);
      fieldObjectArrayList.add(field);
      final var refSchema = totalSchemas.get(cleanRefName(addPropObj));
      if (ApiTool.isEnum(refSchema)) {
        compositedSchemas.put(refSchemaName, SchemaObject
                .builder()
                .className(refSchemaName)
                .isEnum(true)
                .fieldObject(processEnumField(refSchemaName, refSchema, specFile, ApiTool.getEnumValues(refSchema), schema))
                .build());
      }
    } else if (ApiTool.hasItems(addPropObj)) {
      fieldObjectArrayList
          .add(SchemaFieldObject
                   .builder()
                   .baseName(fieldName)
                   .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.ARRAY,
                           MapperUtil.getSimpleType(ApiTool.getItems(addPropObj), specFile)))
                   .build());
    } else if (ApiTool.isObject(addPropObj)) {
      compositedSchemas.putAll(
              mapComponentToSchemaObject(totalSchemas, compositedSchemas, antiLoopList, addPropObj, nameSchema + "Value", specFile,
                      specFile.getModelPackage()));
      fieldObjectArrayList
              .add(SchemaFieldObject
                      .builder()
                      .baseName(ADDITIONAL_PROPERTIES)
                      .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, MapperUtil.getPojoName(nameSchema + "Value", specFile)))
                      .build());
    } else {
      final String type = isBasicType(addPropObj) ? MapperUtil.getSimpleType(addPropObj, specFile)
                        : MapperUtil.getPojoName(ApiTool.getName(schema) + ADDITIONAL_PROPERTY_NAME, specFile);
      fieldObjectArrayList
          .add(SchemaFieldObject
                   .builder()
                   .baseName(fieldName)
                   .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, type))
                   .build());
    }

    return fieldObjectArrayList;
  }

  private static boolean isBasicType(final JsonNode value) {
    return !(ApiTool.isObject(value) || ApiTool.isArray(value) || ApiTool.isComposed(value)
            || ApiTool.hasAdditionalProperties(value));
  }

  private static SchemaObject createComposedSchema(
      final String fieldName, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    String schemaCombinatorType = "";
    if (ApiTool.isAllOf(schema)) {
      fieldObjectArrayList.addAll(
          processAllOf(totalSchemas, ApiTool.getAllOf(schema), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = ALL_OF_COMBINATOR;
    } else if (ApiTool.isAnyOf(schema)) {
      fieldObjectArrayList.addAll(
          processAnyOfOneOf(totalSchemas, ApiTool.getAnyOf(schema), specFile, compositedSchemas, antiLoopList));
      schemaCombinatorType = ANY_OF_COMBINATOR;
    } else if (ApiTool.isOneOf(schema)) {
      fieldObjectArrayList.addAll(
          processAnyOfOneOf(totalSchemas, ApiTool.getOneOf(schema), specFile, compositedSchemas, antiLoopList));
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

  private static void setFieldType(final SchemaFieldObject field, final JsonNode value, final JsonNode schema,
                                   final SpecFile specFile, final String key) {
    field.setRequired(ApiTool.hasRequired(schema) && ApiTool.checkIfRequired(schema, key));
    if (ApiTool.isArray(value)) {
      final String typeArray;
      if (ApiTool.hasItems(value)) {
        typeArray = MapperUtil.getTypeArray(value, specFile);
      } else {
        typeArray = TypeConstants.OBJECT;
      }
      field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, typeArray));
      field.setImportClass(getImportClass(typeArray));
    } else if (ApiTool.hasAdditionalProperties(value)) {
      final String typeObject = getMapTypeObject(value, specFile);
      field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, typeObject));
      field.setImportClass(getImportClass(typeObject));

    } else if (ApiTool.isObject(value)) {
      var typeObject = ApiTool.getType(value);
      if (ApiTool.hasRef(value)) {
        typeObject = getRef(schema, specFile);
      }
      field.setImportClass(getImportClass(typeObject));
      field.getDataType().setDeepType(typeObject);
    }
  }

  private static String getMapTypeObject(final JsonNode schema, final SpecFile specFile) {
    final String type;
    if (ApiTool.isBoolean(ApiTool.getAdditionalProperties(schema))) {
      type = TypeConstants.OBJECT;
    } else {
      final JsonNode additionalProperties = ApiTool.getAdditionalProperties(schema);
      if (ApiTool.hasRef(additionalProperties)) {
        type = getRef(additionalProperties, specFile);
      } else if (ApiTool.isObject(schema)) {
        final var additionalPropertiesField = SchemaFieldObject
                                                  .builder()
                                                  .baseName(ApiTool.getName(additionalProperties))
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

  public static String getRef(final JsonNode schema, final SpecFile specFile) {
    final String typeObject;
    typeObject = MapperUtil.getPojoName(cleanRefName(schema), specFile);
    return typeObject;
  }

  public static String cleanRefName(final String schemaRef) {
    final String[] pathObjectRef = schemaRef.split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  public static String cleanRefName(final JsonNode schema) {
    return cleanRefName(ApiTool.getRefValue(schema));
  }

  private static SchemaFieldObject processEnumField(
          final String key, final JsonNode value, final SpecFile specFile, final List<String> enumValues, final JsonNode schema) {
    final var field = SchemaFieldObject
                          .builder()
                          .baseName(key)
                          .dataType(new SchemaFieldObjectType(TypeConstants.ENUM))
                          .build();
    field.setRequired(schema.has("required")
                      && Objects.nonNull(IteratorUtils.find(schema.get("required").elements(), required -> required.textValue().equalsIgnoreCase(key))));
    final var dataType = MapperUtil.getSimpleType(value, specFile);
    field.getDataType().setDeepType(dataType);

    final HashMap<String, String> enumValuesMap = new HashMap<>();

    for (final var enumValue : enumValues) {
      String valueName = enumValue;
      valueName = valueName.replace(".", "_DOT_");

      switch (dataType) {
        case TypeConstants.INTEGER:
          enumValuesMap.put("INTEGER_" + valueName, enumValue);
          break;
        case TypeConstants.LONG:
          enumValuesMap.put("LONG_" + valueName, enumValue + "l");
          break;
        case TypeConstants.DOUBLE:
          enumValuesMap.put("DOUBLE_" + valueName, enumValue);
          break;
        case TypeConstants.FLOAT:
          enumValuesMap.put("FLOAT_" + valueName, enumValue + "f");
          break;
        case TypeConstants.BIG_DECIMAL:
          enumValuesMap.put("BIG_DECIMAL_" + valueName, "new BigDecimal(\"" + enumValue + "\")");
          break;
        case TypeConstants.STRING:
        default:
          enumValuesMap.put(StringUtils.upperCase(valueName), '"' + enumValue + '"');
          break;
      }
    }

    if (enumValuesMap.isEmpty()) {
      throw new BadDefinedEnumException(key);
    }
    field.setEnumValues(enumValuesMap);
    return field;
  }

  private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !TypeConstants.NO_IMPORT_TYPE.contains(type) ? StringUtils.capitalize(type) : "";
  }

  public static String getDateType(final JsonNode schema, final SpecFile specFile) {
    final String dateType;
    switch (ApiTool.getFormat(schema)) {

      case "date":
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
        break;
      case "date-time":
        switch (specFile.getUseTimeType()) {
          case ZONED:
            dateType = TypeConstants.ZONEDDATETIME;
            break;
          case OFFSET:
            dateType = TypeConstants.OFFSETDATETIME;
            break;
          default:
            dateType = TypeConstants.LOCALDATETIME;
        }
        break;
      default:
        dateType = TypeConstants.LOCALDATETIME;
    }
    return dateType;
  }

}
