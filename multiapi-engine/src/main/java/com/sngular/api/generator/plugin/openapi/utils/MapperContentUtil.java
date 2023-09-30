/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.SchemaUtil;
import com.sngular.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.openapi.model.SchemaObject;
import com.sngular.api.generator.plugin.openapi.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {

  private static final String ADDITIONAL_PROPERTY_NAME = "AdditionalProperty";

  private static final String ANY_OF_COMBINATOR = "anyOf";

  private static final String ONE_OF_COMBINATOR = "oneOf";

  private static final String ALL_OF_COMBINATOR = "allOf";

  private static final String ADDITIONAL_PROPERTIES = "additionalProperties";

  private MapperContentUtil() {
  }

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final JsonNode schema,
      final String schemaName, final SpecFile specFile, final Path baseDir) {

    return mapComponentToSchemaObject(totalSchemas, compositedSchemas, new ArrayList<>(), schema,
                                      StringUtils.defaultIfEmpty(ApiTool.getNodeAsString(schema, "name"), schemaName), specFile, baseDir);
  }

  private static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList, final JsonNode schema, final String schemaName, final SpecFile specFile,
      final Path baseDir) {
    final var name = StringUtils.defaultIfBlank(ApiTool.getNodeAsString(schema, "name"), schemaName);
    if (!compositedSchemas.containsKey(name)) {
      compositedSchemas
        .put(name, toSchemaObject(name, totalSchemas, compositedSchemas, antiLoopList, schema, schemaName, specFile, baseDir));
    }
    return compositedSchemas;
  }

  @SuppressWarnings("checkstyle:ParameterNumber")
  private static SchemaObject toSchemaObject(
      final String name, final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList, final JsonNode schema, final String schemaName, final SpecFile specFile,
      final Path baseDir) {

    antiLoopList.add(schemaName);

    final var listSchema = getFields(totalSchemas, schema, specFile, compositedSchemas, antiLoopList, schemaName, baseDir);

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

    return SchemaObject.builder()
                       .schemaName(name)
                       .className(MapperUtil.getPojoName(schemaName, specFile))
                       .importList(getImportList(listSchema, specFile.getModelPackage()))
                       .schemaCombinator(schemaCombinatorType)
                       .fieldObjectList(listSchema)
                       .build();
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

  private static Set<SchemaFieldObject> getFields(
      final Map<String, JsonNode> totalSchemas, final JsonNode schema, final SpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final String nameSchema,
      final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    if (ApiTool.hasProperties(schema)) {
      if (ApiTool.hasAdditionalProperties(schema)) {
        ApiTool.getProperties(schema)
               .forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList, baseDir));
        fieldObjectArrayList.addAll(
            processAdditionalProperties(ADDITIONAL_PROPERTIES, schema, specFile, totalSchemas, compositedSchemas, antiLoopList, nameSchema, baseDir));
      } else {
        ApiTool.getProperties(schema).forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList, baseDir));
      }
    } else if (TypeConstants.ARRAY.equalsIgnoreCase(ApiTool.getType(schema))) {
      final String itemType = ApiTool.hasRef(ApiTool.getItems(schema)) ? MapperUtil.getRef(ApiTool.getItems(schema), specFile) : ApiTool.getType(ApiTool.getItems(schema));
      fieldObjectArrayList.add(SchemaFieldObject.builder()
                                                .baseName("items")
                                                .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, itemType))
                                                .build());
    } else if (ApiTool.isAllOf(schema)) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, ApiTool.getAllOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isAnyOf(schema)) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, ApiTool.getAnyOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isOneOf(schema)) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, ApiTool.getOneOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isEnum(schema)) {
      fieldObjectArrayList.add(processEnumField(ApiTool.getName(schema), schema, specFile, ApiTool.getEnumValues(schema), schema));
    } else if (ApiTool.hasRef(schema)) {
      final var refSchema = totalSchemas.get(MapperUtil.getRefSchemaName(schema));
      ApiTool.getProperties(refSchema).forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList, baseDir));
    } else {
      fieldObjectArrayList.add(SchemaFieldObject.builder()
                                                .baseName(ApiTool.getName(schema))
                                                .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(schema, specFile)))
                                                .build());
    }

    return fieldObjectArrayList;
  }

  private static Set<SchemaFieldObject> processAllOf(
      final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final SpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    for (JsonNode ref : schemaList) {
      if (ApiTool.hasRef(ref)) {
        final String schemaName = MapperUtil.getRefSchemaName(ref);
        final var schemaToProcess = totalSchemas.get(schemaName);
        ApiTool.getProperties(schemaToProcess).forEachRemaining(processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, ref, antiLoopList, baseDir));
        for (var fieldObject : fieldObjectArrayList) {
          fieldObject.setRequired(true);
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static Set<SchemaFieldObject> processAnyOfOneOf(
      final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final SpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    for (JsonNode internalSchema : schemaList) {
      if (ApiTool.hasRef(internalSchema)) {
        final var schemaName = MapperUtil.getRefSchemaName(internalSchema);
        if (!antiLoopList.contains(schemaName)) {
          if (compositedSchemas.containsKey(schemaName)) {
            antiLoopList.add(schemaName);
            fieldObjectArrayList.addAll(compositedSchemas.get(schemaName).getFieldObjectList());
          } else {
            final var schemaObject = solveRef(internalSchema, totalSchemas, compositedSchemas, antiLoopList, specFile, baseDir);
            fieldObjectArrayList.addAll(schemaObject.getFieldObjectList());
            antiLoopList.add(schemaName);
          }
        } else {
          if (compositedSchemas.containsKey(schemaName)) {
            fieldObjectArrayList.addAll(compositedSchemas.get(schemaName).getFieldObjectList());
          }
        }
      } else {
        fieldObjectArrayList.addAll(getFields(totalSchemas, internalSchema, specFile, compositedSchemas, antiLoopList, ApiTool.getName(internalSchema), baseDir));
        for (var fieldObject : fieldObjectArrayList) {
          if (ApiTool.checkIfRequired(internalSchema, fieldObject.getBaseName())) {
            fieldObject.setRequired(true);
          }
        }
      }
    }
    return fieldObjectArrayList;
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private static List<SchemaFieldObject> processFieldObjectList(
      final String fieldName, final String className, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final Path baseDir) {
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
        schemaObjectComposed = createComposedSchema(StringUtils.defaultIfBlank(className, fieldName), schema, specFile,
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

  private static SchemaFieldObject processRef(
      final String fieldName, final JsonNode schema, final SchemaFieldObjectType dataType,
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList, final SpecFile specFile, final Path baseDir) {
    final var field = SchemaFieldObject.builder()
                                       .baseName(fieldName)
                                       .dataType(dataType)
                                       .build();
    if (!antiLoopList.contains(MapperUtil.getRefSchemaName(schema))) {
      antiLoopList.add(MapperUtil.getRefSchemaName(schema));
      final String refSchemaName = MapperUtil.getRef(schema, specFile);
      setFieldType(field, schema, schema, specFile, refSchemaName);

      solveRef(schema, totalSchemas, compositedSchemas, antiLoopList, specFile, baseDir);
    }
    return field;
  }

  private static SchemaObject solveRef(
      final JsonNode schema, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final SpecFile specFile,
      final Path baseDir) {

    final var referredSchema = SchemaUtil.solveRef(ApiTool.getRefValue(schema), totalSchemas, baseDir.resolve(specFile.getFilePath()).getParent());

    final var schemaObject = toSchemaObject(MapperUtil.getRefSchemaName(schema), totalSchemas, compositedSchemas,
                                            antiLoopList, referredSchema, MapperUtil.getRefSchemaName(schema), specFile, baseDir);
    schemaObject.setEnum(ApiTool.isEnum(referredSchema));

    compositedSchemas.put(MapperUtil.getRefSchemaName(schema), schemaObject);
    return schemaObject;
  }

  private static Consumer<Map.Entry<String, JsonNode>> processProperties(
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas, final Set<SchemaFieldObject> fieldObjectArrayList, final SpecFile specFile,
      final JsonNode schema, final List<String> antiLoopList, final Path baseDir) {
    return field -> {
      final var nodeName = field.getKey();
      final var nodeValue = field.getValue();
      if (ApiTool.isEnum(field.getValue())) {
        fieldObjectArrayList.add(processEnumField(nodeName, nodeValue, specFile, ApiTool.getEnumValues(nodeValue), schema));
      } else {
        fieldObjectArrayList.addAll(processObjectProperty(totalSchemas, nodeName, nodeValue, compositedSchemas, specFile, schema, antiLoopList, baseDir));
      }
    };
  }

  @SuppressWarnings({"checkstyle:CyclomaticComplexity", "checkstyle:ParameterNumber"})
  private static List<SchemaFieldObject> processObjectProperty(
      final Map<String, JsonNode> totalSchemas, final String fieldName, final JsonNode fieldBody, final Map<String, SchemaObject> compositedSchemas, final SpecFile specFile,
      final JsonNode schema, final List<String> antiLoopList, final Path baseDir) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();
    final var isRequired = ApiTool.checkIfRequired(fieldBody, fieldName);
    final SchemaFieldObject field;
    if (ApiTool.hasRef(fieldBody)) {
      final var typeName = MapperUtil.getRefSchemaName(fieldBody);
      final var refSchema = totalSchemas.get(typeName);
      if (!antiLoopList.contains(typeName) && totalSchemas.containsKey(typeName) && ApiTool.hasType(refSchema)
          && ApiTool.hasItems(refSchema) || ApiTool.getRefValue(fieldBody).contains(fieldName)) {
        antiLoopList.add(typeName);
        fieldObjectArrayList.addAll(processFieldObjectList(fieldName, typeName, totalSchemas.get(typeName), specFile, totalSchemas, compositedSchemas, antiLoopList, baseDir));
      } else if (ApiTool.isEnum(refSchema)) {
        fieldObjectArrayList.add(processEnumField(fieldName, refSchema, specFile, ApiTool.getEnumValues(refSchema), schema));
      } else {
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
        .required(isRequired)
        .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(fieldBody, specFile)))
        .build();
      addPropertiesToFieldObject(field, fieldBody);
      setFieldType(field, fieldBody, schema, specFile, fieldName);
      fieldObjectArrayList.add(field);
    } else {
      fieldObjectArrayList.addAll(processFieldObjectList(fieldName, fieldName, fieldBody, specFile, totalSchemas, compositedSchemas, antiLoopList, baseDir));
    }
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject processStringProperty(final String propertyName, final JsonNode schema, final SpecFile specFile) {
    final String resultingType = ApiTool.isDateTime(schema) ? MapperUtil.getDateType(schema, specFile) : TypeConstants.STRING;

    final SchemaFieldObject field = SchemaFieldObject
        .builder()
        .baseName(propertyName)
        .required(ApiTool.checkIfRequired(schema, propertyName))
        .dataType(new SchemaFieldObjectType(resultingType))
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

  @SuppressWarnings("checkstyle:ParameterNumber")
  private static List<SchemaFieldObject> processArray(
      final String fieldName, final String className, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final Path baseDir) {
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
          schemaObjectComposed = createComposedSchema(StringUtils.defaultIfBlank(className, fieldName), items, specFile,
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
        compositedSchemas.putAll(mapComponentToSchemaObject(totalSchemas, compositedSchemas, items, fieldName, specFile, baseDir));
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
      final String fieldName, final String className, final JsonNode schema, final SpecFile specFile,
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final List<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    if (ObjectUtils.allNull(className, fieldName)) {
      ApiTool.getProperties(schema).forEachRemaining(
          processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList, baseDir));
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
                                   baseDir));
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
      final String fieldName, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    if (TypeConstants.OBJECT.equalsIgnoreCase(ApiTool.getType(schema)) && ApiTool.hasProperties(schema)) {
      ApiTool.getProperties(schema).forEachRemaining(
           processProperties(totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList, baseDir));
    }

    if (ApiTool.hasAdditionalProperties(schema)) {
      fieldObjectArrayList.addAll(processAdditionalProperties(fieldName, schema, specFile, totalSchemas, compositedSchemas,
                                                              antiLoopList, ADDITIONAL_PROPERTIES, baseDir));
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAdditionalProperties(
      final String fieldName, final JsonNode schema, final SpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final List<String> antiLoopList, final String nameSchema,
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
      final String refSchemaName = MapperUtil.getRef(addPropObj, specFile);
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
      compositedSchemas.putAll(
           mapComponentToSchemaObject(totalSchemas, compositedSchemas, antiLoopList, addPropObj, nameSchema + "Value", specFile,
                                   baseDir));
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
      final List<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();
    String schemaCombinatorType = "";
    if (ApiTool.isAllOf(schema)) {
      fieldObjectArrayList.addAll(
           processAllOf(totalSchemas, ApiTool.getAllOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
      schemaCombinatorType = ALL_OF_COMBINATOR;
    } else if (ApiTool.isAnyOf(schema)) {
      fieldObjectArrayList.addAll(
           processAnyOfOneOf(totalSchemas, ApiTool.getAnyOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
      schemaCombinatorType = ANY_OF_COMBINATOR;
    } else if (ApiTool.isOneOf(schema)) {
      fieldObjectArrayList.addAll(
           processAnyOfOneOf(totalSchemas, ApiTool.getOneOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
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
      final SpecFile specFile, final String key) {
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
        typeObject = MapperUtil.getRef(schema, specFile);
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
        type = MapperUtil.getRef(additionalProperties, specFile);
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

  private static SchemaFieldObject processEnumField(
       final String key, final JsonNode value, final SpecFile specFile, final List<String> enumValues, final JsonNode schema) {
    final var field = SchemaFieldObject
        .builder()
        .baseName(key)
        .dataType(new SchemaFieldObjectType(TypeConstants.ENUM))
        .build();
    field.setRequired(ApiTool.checkIfRequired(schema, key));
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

}
