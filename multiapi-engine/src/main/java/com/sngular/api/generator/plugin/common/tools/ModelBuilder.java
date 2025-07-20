/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.*;
import com.sngular.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;

import java.nio.file.Path;
import java.util.*;
import java.util.function.Consumer;

public final class ModelBuilder {

  private static final String ADDITIONAL_PROPERTY_NAME = "AdditionalProperty";

  private static final String ANY_OF_COMBINATOR = "anyOf";

  private static final String ONE_OF_COMBINATOR = "oneOf";

  private static final String ALL_OF_COMBINATOR = "allOf";

  private static final String ADDITIONAL_PROPERTIES = "additionalProperties";

  private static final Map<String, SchemaObject> cachedSchemas = new HashMap<>();

  private ModelBuilder() {
  }

  public static SchemaObject buildSchemaObject(
        final Map<String, JsonNode> totalSchemas, final String className, final JsonNode model,
        final Set<String> antiLoopList, final Map<String, SchemaObject> compositedSchemas, final String parentPackage,
        final CommonSpecFile specFile, final Path baseDir) {

    antiLoopList.add(WordUtils.capitalizeFully(className));
    final var schemaBuilder = SchemaObject.builder()
      .schemaName(WordUtils.capitalizeFully(className));
    final var calculatedInlinePrefix = MapperUtil.calculatePrefixName("Inline", specFile);
    if (!StringUtils.startsWith(className, calculatedInlinePrefix)) {
      schemaBuilder.className(MapperUtil.getPojoName(className, specFile));
    } else {
      schemaBuilder.className(className);
    }

    if (!ApiTool.isEnum(model)) {
      final var listSchema = getFields(null, totalSchemas, model, specFile, compositedSchemas, antiLoopList, WordUtils.capitalizeFully(className), baseDir);

      schemaBuilder.importList(getImportList(listSchema, specFile.getModelPackage()));

      if (ApiTool.isAllOf(model)) {
        schemaBuilder.schemaCombinator(ALL_OF_COMBINATOR);
      } else if (ApiTool.isAnyOf(model)) {
        schemaBuilder.schemaCombinator(ANY_OF_COMBINATOR);
      } else if (ApiTool.isOneOf(model)) {
        schemaBuilder.schemaCombinator(ONE_OF_COMBINATOR);
      } else {
        schemaBuilder.schemaCombinator("");
      }

       schemaBuilder
        .fieldObjectList(listSchema)
        .parentPackage(parentPackage.toLowerCase());
    } else {
      schemaBuilder
        .isEnum(true)
        .fieldObject(processEnumField(className, model, specFile, ApiTool.getEnumValues(model), model));
    }
    cachedSchemas.putAll(compositedSchemas);
    return schemaBuilder.build();
  }

  private static List<String> getImportList(final Set<SchemaFieldObject> fieldObjectList, final String modelPackage) {
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

  private static void getTypeImports(
        final HashMap<String, List<String>> listHashMap,
        final SchemaFieldObject fieldObject) {
    final SchemaFieldObjectType type = fieldObject.getDataType();
    if (type.containsType(TypeConstants.ARRAY)) {
      listHashMap.computeIfAbsent(TypeConstants.ARRAY, key -> List.of("java.util.List", "java.util.ArrayList"));
    }

    if (type.containsType(TypeConstants.MAP)) {
      listHashMap.computeIfAbsent(TypeConstants.MAP, key -> List.of("java.util.Map", "java.util.HashMap"));
    }

    if (type.containsType(TypeConstants.BIG_DECIMAL)) {
      listHashMap.computeIfAbsent(TypeConstants.BIG_DECIMAL, key -> Collections.singletonList("java.math.BigDecimal"));
    }

    if (type.containsType(TypeConstants.LOCALDATE)) {
      listHashMap.computeIfAbsent(TypeConstants.LOCALDATE, key -> Collections.singletonList("java.time.LocalDate"));
    }

    if (type.containsType(TypeConstants.LOCALDATETIME)) {
      listHashMap.computeIfAbsent(TypeConstants.LOCALDATETIME, key -> Collections.singletonList("java.time.LocalDateTime"));
    }

    if (type.containsType(TypeConstants.ZONEDDATE)) {
      listHashMap.computeIfAbsent(TypeConstants.ZONEDDATETIME, key -> Collections.singletonList("java.time.ZonedDateTime"));
    }

    if (type.containsType(TypeConstants.ZONEDDATETIME)) {
      listHashMap.computeIfAbsent(TypeConstants.ZONEDDATETIME, key -> Collections.singletonList("java.time.ZonedDateTime"));
    }

    if (type.containsType(TypeConstants.OFFSETDATE)) {
      listHashMap.computeIfAbsent(TypeConstants.OFFSETDATETIME, key -> Collections.singletonList("java.time.OffsetDateTime"));
    }

    if (type.containsType(TypeConstants.OFFSETDATETIME)) {
      listHashMap.computeIfAbsent(TypeConstants.OFFSETDATETIME, key -> Collections.singletonList("java.time.OffsetDateTime"));
    }

    if (type.containsType(TypeConstants.MULTIPART_FILE)) {
      listHashMap.computeIfAbsent(TypeConstants.MULTIPART_FILE, key -> Collections.singletonList("org.springframework.web.multipart.MultipartFile"));
    }
  }

  private static Set<SchemaFieldObject> getFields(final String buildingSchema,
                                                  final Map<String, JsonNode> totalSchemas, final JsonNode schema, final CommonSpecFile specFile,
                                                  final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final String nameSchema,
                                                  final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    if (ApiTool.hasProperties(schema)) {
      if (ApiTool.hasAdditionalProperties(schema)) {
        ApiTool.getProperties(schema).forEachRemaining(processProperties(buildingSchema, totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList,
              baseDir));
        fieldObjectArrayList.addAll(processAdditionalProperties(ADDITIONAL_PROPERTIES, schema, specFile, totalSchemas, compositedSchemas, antiLoopList, nameSchema, baseDir));
      } else {
        ApiTool.getProperties(schema).forEachRemaining(processProperties(nameSchema, totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList,
              baseDir));
      }
    } else if (TypeConstants.ARRAY.equalsIgnoreCase(ApiTool.getType(schema))) {
      final String itemType = ApiTool.hasRef(ApiTool.getItems(schema)) ? MapperUtil.getPojoNameFromRef(ApiTool.getItems(schema), specFile, null) : ApiTool.getType(ApiTool.getItems(schema));
      fieldObjectArrayList.add(SchemaFieldObject.builder()
            .baseName("items")
            .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, itemType))
            .build());
    } else if (ApiTool.isAllOf(schema)) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, ApiTool.getAllOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isAnyOf(schema)) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(buildingSchema, totalSchemas, ApiTool.getAnyOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isOneOf(schema)) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(buildingSchema, totalSchemas, ApiTool.getOneOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isEnum(schema)) {
      fieldObjectArrayList.add(processEnumField(ApiTool.getName(schema), schema, specFile, ApiTool.getEnumValues(schema), schema));
    } else if (ApiTool.hasRef(schema)) {
      final var refSchema = totalSchemas.get(MapperUtil.getRefSchemaKey(schema));
      ApiTool.getProperties(refSchema).forEachRemaining(processProperties(buildingSchema, totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, refSchema, antiLoopList,
            baseDir));
    } else {
      fieldObjectArrayList.add(SchemaFieldObject.builder()
            .baseName(ApiTool.getName(schema))
            .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(schema, specFile)))
            .build());
    }

    return fieldObjectArrayList;
  }


  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private static List<SchemaFieldObject> processFieldObjectList(final String buildingSchema,
                                                                final String fieldName, final String className, final JsonNode schema, final CommonSpecFile specFile,
                                                                final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
                                                                final Set<String> antiLoopList, final Path baseDir) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (TypeConstants.ARRAY.equalsIgnoreCase(ApiTool.getType(schema))) {
      fieldObjectArrayList.addAll(processArray(fieldName, className, schema, specFile, totalSchemas, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.hasAdditionalProperties(schema)) {
      fieldObjectArrayList.addAll(processMap(fieldName, schema, specFile, totalSchemas, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.hasRef(schema)) {
      fieldObjectArrayList.add(
            processRef(fieldName, schema, new SchemaFieldObjectType(MapperUtil.getSimpleType(schema, specFile)), totalSchemas, compositedSchemas, antiLoopList, specFile, baseDir));
    } else if (ApiTool.isObject(schema) && !ApiTool.hasProperties(schema)) {
      fieldObjectArrayList.add(SchemaFieldObject
            .builder()
            .baseName(fieldName)
            .dataType(new SchemaFieldObjectType(TypeConstants.OBJECT))
            .build());
    } else if (ApiTool.isEnum(schema)) {
      fieldObjectArrayList.add(processEnumField(fieldName, schema, specFile, ApiTool.getEnumValues(schema), schema));
    } else if (!ApiTool.hasProperties(schema) && !ApiTool.isComposed(schema)) {
      fieldObjectArrayList.add(SchemaFieldObject
            .builder()
            .baseName(fieldName)
            .dataType(new SchemaFieldObjectType(TypeConstants.OBJECT))
            .build());
    } else if (ApiTool.isObject(schema)) {
      fieldObjectArrayList.addAll(processObject(fieldName, className, schema, specFile, totalSchemas, compositedSchemas, antiLoopList, baseDir));
    } else {
      final var composedSchemaName = StringUtils.defaultIfBlank(className, fieldName);
      var schemaObjectComposed = compositedSchemas.get(composedSchemaName);
      if (Objects.isNull(schemaObjectComposed)) {
        schemaObjectComposed = createComposedSchema(buildingSchema, StringUtils.defaultIfBlank(className, fieldName), schema, specFile,
              totalSchemas, compositedSchemas, antiLoopList, baseDir);
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

  private static Consumer<Map.Entry<String, JsonNode>> processProperties(final String buildingSchema,
                                                                         final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final Set<SchemaFieldObject> fieldObjectArrayList, final CommonSpecFile specFile,
                                                                         final JsonNode schema, final Set<String> antiLoopList, final Path baseDir) {
    return field -> {
      final var nodeName = field.getKey();
      final var nodeValue = field.getValue();
      if (ApiTool.isEnum(field.getValue())) {
        fieldObjectArrayList.add(processEnumField(nodeName, nodeValue, specFile, ApiTool.getEnumValues(nodeValue), schema));
      } else {
        fieldObjectArrayList.addAll(processObjectProperty(buildingSchema, totalSchemas, nodeName, nodeValue, compositedSchemas, specFile, schema, antiLoopList, baseDir));
      }
    };
  }

  @SuppressWarnings({"checkstyle:CyclomaticComplexity", "checkstyle:ParameterNumber"})
  private static List<SchemaFieldObject> processObjectProperty(final String buildingSchema,
                                                               final Map<String, JsonNode> totalSchemas, final String fieldName, final JsonNode fieldBody, final Map<String, SchemaObject> compositedSchemas, final CommonSpecFile specFile,
                                                               final JsonNode schema, final Set<String> antiLoopList, final Path baseDir) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();
    final var isRequired = ApiTool.checkIfRequired(fieldBody, fieldName);
    final SchemaFieldObject field;
    if (ApiTool.hasRef(fieldBody)) {
      final var typeName = MapperUtil.getRefSchemaName(fieldBody, fieldName);
      final var refSchema = totalSchemas.get(MapperUtil.getRefSchemaKey(fieldBody));
      if (!antiLoopList.contains(typeName) && Objects.nonNull(refSchema) && ApiTool.hasType(refSchema)
            && ApiTool.hasItems(refSchema) || ApiTool.getRefValue(fieldBody).contains(fieldName)) {
        if (antiLoopList.contains(typeName) && ApiTool.getRefValue(fieldBody).contains(fieldName)) {
          fieldObjectArrayList.add(SchemaFieldObject
            .builder()
            .baseName(fieldName)
            .required(ApiTool.checkIfRequired(schema, fieldName))
            .dataType(SchemaFieldObjectType.fromTypeList(MapperUtil.getSimpleType(refSchema, specFile),
              MapperUtil.getPojoName(typeName, specFile)))
            .build());
        } else {
        antiLoopList.add(typeName);
        fieldObjectArrayList.addAll(processFieldObjectList(buildingSchema, fieldName, typeName, refSchema, specFile, totalSchemas, compositedSchemas,
              antiLoopList, baseDir));
        }
      } else if (ApiTool.isEnum(refSchema)) {
        fieldObjectArrayList.add(processEnumField(fieldName, refSchema, specFile, ApiTool.getEnumValues(refSchema), schema));
      } else if (ApiTool.isObject(refSchema) || ApiTool.isComposed(refSchema)) {
          compositedSchemas.put(typeName, buildSchemaObject(totalSchemas, typeName, refSchema, antiLoopList, compositedSchemas, "", specFile, baseDir ));
          fieldObjectArrayList.add(SchemaFieldObject
            .builder()
            .baseName(fieldName)
            .required(ApiTool.checkIfRequired(schema, fieldName))
            .dataType(SchemaFieldObjectType.fromTypeList(MapperUtil.getSimpleType(refSchema, specFile),
              MapperUtil.getPojoName(typeName, specFile)))
            .build());
      } else if (ApiTool.isBoolean(refSchema) || ApiTool.isString(refSchema) || ApiTool.isNumber(refSchema) || ApiTool.isDateTime(refSchema)) {
        fieldObjectArrayList.add(SchemaFieldObject
          .builder()
          .baseName(fieldName)
          .required(ApiTool.checkIfRequired(schema, fieldName))
          .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(refSchema, specFile)))
          .constValue(ApiTool.getConst(refSchema))
          .build());
      } else if (antiLoopList.contains(typeName) &&
          cachedSchemas.containsKey(typeName)) {
          fieldObjectArrayList.add(SchemaFieldObject
            .builder()
            .baseName(fieldName)
            .required(ApiTool.checkIfRequired(schema, fieldName))
            .dataType(SchemaFieldObjectType.fromTypeList(MapperUtil.getSimpleType(refSchema, specFile),
              MapperUtil.getPojoName(typeName, specFile)))
            .build());
      }
    } else if (ApiTool.isEnum(fieldBody)) {
      fieldObjectArrayList.add(processEnumField(fieldName, fieldBody, specFile, ApiTool.getEnumValues(fieldBody), fieldBody));
    } else if (TypeConstants.STRING.equalsIgnoreCase(ApiTool.getType(fieldBody))) {
      field = processStringProperty(fieldName, fieldBody, specFile);
      setFieldType(field, fieldBody, schema, specFile, fieldName);
      fieldObjectArrayList.add(field);
    } else if (isBasicType(fieldBody)) {
      field = SchemaFieldObject
            .builder()
            .baseName(fieldName)
            .required(isRequired || ApiTool.hasConst(fieldBody))
            .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(fieldBody, specFile)))
            .constValue(ApiTool.getConst(fieldBody))
            .build();
      addPropertiesToFieldObject(field, fieldBody);
      setFieldType(field, fieldBody, schema, specFile, fieldName);
      fieldObjectArrayList.add(field);
    } else {
      fieldObjectArrayList.addAll(processFieldObjectList(buildingSchema, fieldName, fieldName, fieldBody, specFile, totalSchemas, compositedSchemas, antiLoopList, baseDir));
    }
    return fieldObjectArrayList;
  }

  private static Object getConst(final JsonNode fieldBody) {
    return ApiTool.hasConst(fieldBody) ? ApiTool.getConst(fieldBody) : null;
  }

  private static SchemaFieldObject processStringProperty(final String propertyName, final JsonNode schema, final CommonSpecFile specFile) {
    String resultingType;
    if (ApiTool.isDateTime(schema)) {
      resultingType = MapperUtil.getDateType(schema, specFile);
    }
    else if (ApiTool.isBinary(schema)) {
      resultingType = TypeConstants.MULTIPART_FILE;
    }
    else {
      resultingType = TypeConstants.STRING;
    }

    final SchemaFieldObject field = SchemaFieldObject
          .builder()
          .baseName(propertyName)
          .required(ApiTool.checkIfRequired(schema, propertyName) || ApiTool.hasConst(schema))
          .dataType(new SchemaFieldObjectType(resultingType))
          .constValue(ApiTool.getConst(schema))
          .build();
    addPropertiesToFieldObject(field, schema);
    return field;
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private static void addPropertiesToFieldObject(final SchemaFieldObject fieldObject, final JsonNode value) {
    final var restrictionList = ApiTool.getFieldIterator(value);
    while (restrictionList.hasNext()) {
      final var restriction = restrictionList.next();
      switch (restriction.getKey()) {
        case "pattern":
          fieldObject.getRestrictions().setPattern(restriction.getValue().asText());
          break;
        case "maxItems":
          fieldObject.getRestrictions().setMaxItems(restriction.getValue().asInt());
          break;
        case "minItems":
          fieldObject.getRestrictions().setMinItems(restriction.getValue().asInt());
          break;
        case "maxLength":
          fieldObject.getRestrictions().setMaxLength(restriction.getValue().asInt());
          break;
        case "minLength":
          fieldObject.getRestrictions().setMinLength(restriction.getValue().asInt());
          break;
        case "uniqueItems":
          fieldObject.getRestrictions().setUniqueItems(restriction.getValue().asBoolean());
          break;
        case "exclusiveMaximum":
          if (restriction.getValue().isBoolean()) {
            fieldObject.getRestrictions().setExclusiveMaximum(restriction.getValue().asBoolean());
          } else if (restriction.getValue().isNumber()) {
            fieldObject.getRestrictions().setMaximum(restriction.getValue().asText());
            fieldObject.getRestrictions().setExclusiveMaximum(true);
          }
          break;
        case "exclusiveMinimum":
          if (restriction.getValue().isBoolean()) {
            fieldObject.getRestrictions().setExclusiveMinimum(restriction.getValue().asBoolean());
          } else if (restriction.getValue().isNumber()) {
            fieldObject.getRestrictions().setMinimum(restriction.getValue().asText());
            fieldObject.getRestrictions().setExclusiveMinimum(true);
          }
          break;
        case "multipleOf":
          fieldObject.getRestrictions().setMultipleOf(restriction.getValue().asText());
          break;
        case "maximum":
          fieldObject.getRestrictions().setMaximum(restriction.getValue().asText());
          break;
        case "minimum":
          fieldObject.getRestrictions().setMinimum(restriction.getValue().asText());
          break;
        default:
          break;
      }
    }
  }

  @SuppressWarnings("checkstyle:ParameterNumber")
  private static List<SchemaFieldObject> processArray(
        final String fieldName, final String className, final JsonNode schema, final CommonSpecFile specFile, final Map<String, JsonNode> totalSchemas,
        final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final Path baseDir) {
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
        fieldObjectArrayList.add(
              processRef(fieldName, items, SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, MapperUtil.getSimpleType(items, specFile)), totalSchemas, compositedSchemas,
                    antiLoopList, specFile, baseDir));
      } else if (ApiTool.isComposed(items)) {
        final String composedSchemaName = StringUtils.defaultIfBlank(className, fieldName);
        SchemaObject schemaObjectComposed = compositedSchemas.get(composedSchemaName);
        if (Objects.isNull(schemaObjectComposed)) {
          schemaObjectComposed = createComposedSchema("", StringUtils.defaultIfBlank(className, fieldName), items, specFile,
                totalSchemas, compositedSchemas, antiLoopList, baseDir);
          compositedSchemas.put(composedSchemaName, schemaObjectComposed);
        }

        fieldObjectArrayList.add(SchemaFieldObject
              .builder()
              .baseName(fieldName)
              .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, schemaObjectComposed.getClassName()))
              .importClass(schemaObjectComposed.getClassName())
              .build());
      } else if (ApiTool.hasProperties(items)) {
        final var itemsObject = buildSchemaObject(totalSchemas, className, items, antiLoopList, compositedSchemas, "", specFile, baseDir);
        compositedSchemas.put(className, itemsObject);
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

  @SuppressWarnings("checkstyle:ParameterNumber")
  private static Set<SchemaFieldObject> processObject(
        final String fieldName, final String className, final JsonNode schema, final CommonSpecFile specFile,
        final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
        final Set<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    if (ObjectUtils.allNull(className, fieldName)) {
      ApiTool.getProperties(schema).forEachRemaining(
            processProperties("", totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList, baseDir));
    } else if (antiLoopList.contains(className) && compositedSchemas.containsKey(className)) {
      fieldObjectArrayList
            .add(SchemaFieldObject
                  .builder()
                  .baseName(className)
                  .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, MapperUtil.getPojoName(className, specFile)))
                  .build());
    } else if (antiLoopList.contains(fieldName) && compositedSchemas.containsKey(className)) {
      fieldObjectArrayList
            .add(SchemaFieldObject
                  .builder()
                  .baseName(fieldName)
                  .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, MapperUtil.getPojoName(fieldName, specFile)))
                  .build());
    } else {
      final String name = StringUtils.defaultIfBlank(className, fieldName);
      final var itemsObject = buildSchemaObject(totalSchemas, className, schema, antiLoopList, compositedSchemas, "", specFile, baseDir);
      compositedSchemas.put(className, itemsObject);
      fieldObjectArrayList
            .add(SchemaFieldObject
                  .builder()
                  .baseName(name)
                  .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, MapperUtil.getPojoName(name, specFile)))
                  .build());
    }

    return fieldObjectArrayList;
  }

  private static Set<SchemaFieldObject> processMap(
        final String fieldName, final JsonNode schema, final CommonSpecFile specFile, final Map<String, JsonNode> totalSchemas,
        final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    if (TypeConstants.OBJECT.equalsIgnoreCase(ApiTool.getType(schema)) && ApiTool.hasProperties(schema)) {
      ApiTool.getProperties(schema).forEachRemaining(
            processProperties("", totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList, baseDir));
    }

    if (ApiTool.hasAdditionalProperties(schema)) {
      fieldObjectArrayList.addAll(processAdditionalProperties(fieldName, schema, specFile, totalSchemas, compositedSchemas,
            antiLoopList, ADDITIONAL_PROPERTIES, baseDir));
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAdditionalProperties(
        final String fieldName, final JsonNode schema, final CommonSpecFile specFile, final Map<String, JsonNode> totalSchemas,
        final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final String nameSchema,
        final Path baseDir) {
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
      final String refSchemaName = MapperUtil.getPojoNameFromRef(addPropObj, specFile, null);
      fieldObjectArrayList.add(processRef(fieldName, addPropObj,
            SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, refSchemaName), totalSchemas, compositedSchemas,
            antiLoopList, specFile, baseDir));
    } else if (ApiTool.hasItems(addPropObj)) {
      fieldObjectArrayList
            .add(SchemaFieldObject
                  .builder()
                  .baseName(fieldName)
                  .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.ARRAY,
                        MapperUtil.getSimpleType(ApiTool.getItems(addPropObj), specFile)))
                  .build());
    } else if (ApiTool.isObject(addPropObj)) {
      final String className = nameSchema + "Value";
      final var itemsObject = buildSchemaObject(totalSchemas, className, addPropObj, antiLoopList, compositedSchemas, "", specFile, baseDir);
      compositedSchemas.put(className, itemsObject);
      fieldObjectArrayList
            .add(SchemaFieldObject
                  .builder()
                  .baseName(ADDITIONAL_PROPERTIES)
                  .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, MapperUtil.getPojoName(className, specFile)))
                  .build());
    } else {
      final String type = isBasicType(addPropObj) ? MapperUtil.getSimpleType(addPropObj, specFile)
            : MapperUtil.getPojoName(ApiTool.getName(schema) + ADDITIONAL_PROPERTY_NAME, specFile);
      fieldObjectArrayList
            .add(SchemaFieldObject
                  .builder()
                  .baseName(fieldName)
                  .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, type))
                  .constValue(getConst(addPropObj))
                  .required(ApiTool.checkIfRequired(schema, fieldName) ||ApiTool.hasConst(addPropObj))
                  .build());
    }

    return fieldObjectArrayList;
  }

  private static boolean isBasicType(final JsonNode value) {
    return !(ApiTool.isObject(value) || ApiTool.isArray(value) || ApiTool.isComposed(value)
          || ApiTool.hasAdditionalProperties(value));
  }

  private static SchemaObject createComposedSchema(final String buildingSchema,
                                                   final String fieldName, final JsonNode schema, final CommonSpecFile specFile, final Map<String, JsonNode> totalSchemas,
                                                   final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();
    String schemaCombinatorType = "";
    if (ApiTool.isAllOf(schema)) {
      fieldObjectArrayList.addAll(
            processAllOf(totalSchemas, ApiTool.getAllOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
      schemaCombinatorType = ALL_OF_COMBINATOR;
    } else if (ApiTool.isAnyOf(schema)) {
      fieldObjectArrayList.addAll(
        processAnyOfOneOf(fieldName, totalSchemas, ApiTool.getAnyOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
      schemaCombinatorType = ANY_OF_COMBINATOR;
    } else if (ApiTool.isOneOf(schema)) {
      fieldObjectArrayList.addAll(
            processAnyOfOneOf(buildingSchema, totalSchemas, ApiTool.getOneOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
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

  private static void setFieldType(
        final SchemaFieldObject field, final JsonNode schemaProperty, final JsonNode schema,
        final CommonSpecFile specFile, final String key) {
    field.setRequired(ApiTool.hasRequired(schema) && ApiTool.checkIfRequired(schema, key));
    if (ApiTool.isArray(schemaProperty)) {
      final String typeArray;
      if (ApiTool.hasItems(schemaProperty)) {
        typeArray = MapperUtil.getTypeArray(schemaProperty, specFile);
      } else {
        typeArray = TypeConstants.OBJECT;
      }
      field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, typeArray));
      field.setImportClass(getImportClass(typeArray));
    } else if (ApiTool.hasAdditionalProperties(schemaProperty)) {
      final String typeObject = getMapTypeObject(schemaProperty, specFile);
      field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, typeObject));
      field.setImportClass(getImportClass(typeObject));

    } else if (ApiTool.isObject(schemaProperty)) {
      var typeObject = ApiTool.getType(schemaProperty);
      if (ApiTool.hasRef(schemaProperty)) {
        typeObject = MapperUtil.getPojoNameFromRef(schema, specFile, null);
      }
      field.setImportClass(getImportClass(typeObject));
      field.getDataType().setDeepType(typeObject);
    }
  }

  private static String getMapTypeObject(final JsonNode schema, final CommonSpecFile specFile) {
    final String type;
    if (ApiTool.isBoolean(ApiTool.getAdditionalProperties(schema))) {
      type = TypeConstants.OBJECT;
    } else {
      final JsonNode additionalProperties = ApiTool.getAdditionalProperties(schema);
      if (ApiTool.hasRef(additionalProperties)) {
        type = MapperUtil.getPojoNameFromRef(additionalProperties, specFile, null);
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

    return switch (fieldType) {
      case TypeConstants.BIG_DECIMAL, TypeConstants.INTEGER, TypeConstants.DOUBLE, TypeConstants.FLOAT,
           TypeConstants.LONG, TypeConstants.STRING -> fieldType;
      default -> TypeConstants.OBJECT;
    };
  }

  private static Set<SchemaFieldObject> processAllOf(
        final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final CommonSpecFile specFile,
        final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    for (JsonNode ref : schemaList) {
      if (ApiTool.hasRef(ref)) {
        final var schemaToProcess = totalSchemas.get(MapperUtil.getRefSchemaKey(ref));
        ApiTool.getProperties(schemaToProcess).forEachRemaining(processProperties("", totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, ref, antiLoopList, baseDir));
        for (var fieldObject : fieldObjectArrayList) {
          fieldObject.setRequired(true);
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static Set<SchemaFieldObject> processAnyOfOneOf(final String buildingSchema,
                                                          final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final CommonSpecFile specFile,
                                                          final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    for (JsonNode internalSchema : schemaList) {
      if (ApiTool.hasRef(internalSchema)) {
        final var schemaName = MapperUtil.getRefSchemaName(internalSchema, null);
        if (!antiLoopList.contains(schemaName)) {
          if (compositedSchemas.containsKey(schemaName)) {
            antiLoopList.add(schemaName);
            fieldObjectArrayList.addAll(compositedSchemas.get(schemaName).getFieldObjectList());
          } else {
            antiLoopList.add(schemaName);
            final var schemaObject = solveRef(internalSchema, totalSchemas, compositedSchemas, antiLoopList, specFile, baseDir);
            fieldObjectArrayList.addAll(schemaObject.getFieldObjectList());
          }
        } else if (compositedSchemas.containsKey(schemaName)) {
          fieldObjectArrayList.addAll(compositedSchemas.get(schemaName).getFieldObjectList());
        } else if (!schemaName.equalsIgnoreCase(buildingSchema)) {
          fieldObjectArrayList.add(SchemaFieldObject.builder()
            .baseName(schemaName)
            .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(internalSchema, specFile)))
            .build());
        }
      } else {
        fieldObjectArrayList.addAll(getFields(buildingSchema, totalSchemas, internalSchema, specFile, compositedSchemas, antiLoopList, ApiTool.getName(internalSchema), baseDir));
        for (var fieldObject : fieldObjectArrayList) {
          if (ApiTool.checkIfRequired(internalSchema, fieldObject.getBaseName())) {
            fieldObject.setRequired(true);
          }
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject processRef(
        final String fieldName, final JsonNode schema, final SchemaFieldObjectType dataType,
        final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
        final Set<String> antiLoopList, final CommonSpecFile specFile, final Path baseDir) {
    final var field = SchemaFieldObject.builder()
          .baseName(fieldName)
          .dataType(dataType)
          .build();
    if (!antiLoopList.contains(MapperUtil.getRefSchemaName(schema, fieldName))) {
      antiLoopList.add(MapperUtil.getRefSchemaName(schema, fieldName));
      final String refSchemaName = MapperUtil.getPojoNameFromRef(schema, specFile, fieldName);
      setFieldType(field, schema, schema, specFile, refSchemaName);

      solveRef(schema, totalSchemas, compositedSchemas, antiLoopList, specFile, baseDir);
    }
    return field;
  }

  private static SchemaObject solveRef(
        final JsonNode schema, final Map<String, JsonNode> totalSchemas,
        final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final CommonSpecFile specFile,
        final Path baseDir) {

    final var referredSchema = SchemaUtil.solveRef(ApiTool.getRefValue(schema), totalSchemas, baseDir.resolve(specFile.getFilePath()).getParent().toUri());

    final var schemaObject = buildSchemaObject(totalSchemas, MapperUtil.getRefSchemaName(schema, null), referredSchema,
          antiLoopList, compositedSchemas, MapperUtil.getRefSchemaName(schema, null), specFile, baseDir);
    schemaObject.setEnum(ApiTool.isEnum(referredSchema));

    compositedSchemas.put(MapperUtil.getRefSchemaName(schema, null), schemaObject);
    return schemaObject;
  }

  private static SchemaFieldObject processEnumField(
        final String name, final JsonNode value,
        final CommonSpecFile specFile, final List<String> enumValues, final JsonNode schema) {
    final var field = SchemaFieldObject
          .builder()
          .baseName(name)
          .dataType(new SchemaFieldObjectType(TypeConstants.ENUM))
          .build();
    field.setRequired(ApiTool.checkIfRequired(schema, name));
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
          enumValuesMap.put(StringUtils.replace(StringUtils.upperCase(valueName), " ", "_"),
                  '"' + enumValue + '"');
          break;
      }
    }

    if (enumValuesMap.isEmpty()) {
      throw new BadDefinedEnumException(name);
    }
    field.setEnumValues(enumValuesMap);
    return field;
  }

  private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !TypeConstants.NO_IMPORT_TYPE.contains(type) ? StringUtils.capitalize(type) : "";
  }


}
