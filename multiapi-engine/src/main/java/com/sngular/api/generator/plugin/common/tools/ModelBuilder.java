/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;

@Slf4j
public final class ModelBuilder {

  private static final String ADDITIONAL_PROPERTY_NAME = "AdditionalProperty";

  private static final String ANY_OF_COMBINATOR = "anyOf";

  private static final String ONE_OF_COMBINATOR = "oneOf";

  private static final String ALL_OF_COMBINATOR = "allOf";

  private static final String ADDITIONAL_PROPERTIES = "additionalProperties";

  /** Rendered types that say nothing about the value they hold, as a template would print them. */
  private static final Set<String> FREE_FORM_TYPES = Set.of("Object", "List<Object>", "Map<String, Object>");

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

  private static Set<SchemaFieldObject> getFields(
      final String buildingSchema,
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
      fieldObjectArrayList.add(SchemaFieldObject.builder()
                                                .baseName("items")
                                                .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, resolveArrayItemType(schema, specFile)))
                                                .build());
    } else if (ApiTool.hasPatternProperties(schema)) {
      fieldObjectArrayList.add(buildPatternPropertiesField(resolveFieldName(schema, nameSchema), schema, specFile));
    } else if (ApiTool.isAllOf(schema)) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, schema, specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isAnyOf(schema)) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(buildingSchema, totalSchemas, ApiTool.getAnyOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isOneOf(schema)) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(buildingSchema, totalSchemas, ApiTool.getOneOf(schema), specFile, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.isEnum(schema)) {
      fieldObjectArrayList.add(processEnumField(resolveFieldName(schema, nameSchema), schema, specFile, ApiTool.getEnumValues(schema), schema));
    } else if (ApiTool.hasRef(schema)) {
      final var refSchema = totalSchemas.get(MapperUtil.getRefSchemaKey(schema));
      ApiTool.getProperties(refSchema).forEachRemaining(processProperties(buildingSchema, totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, refSchema, antiLoopList,
                                                                          baseDir));
    } else if (ApiTool.isInlineObject(schema)) {
      schema.fields().forEachRemaining(processProperties(nameSchema, totalSchemas, compositedSchemas, fieldObjectArrayList, specFile, schema, antiLoopList, baseDir));
    } else {
      fieldObjectArrayList.add(SchemaFieldObject.builder()
                                                .baseName(resolveFieldName(schema, nameSchema))
                                                .dataType(new SchemaFieldObjectType(MapperUtil.getSimpleType(schema, specFile)))
                                                .build());
    }

    // Every template interpolates the field name, so a field without one cannot be rendered at all:
    // it is a schema that contributes no named property (e.g. the bare `{"type": "object"}` member
    // of an `anyOf`), and dropping it keeps the surrounding model generable. The consequence is
    // reported where it becomes visible — the schema that ends up with nothing to model.
    fieldObjectArrayList.removeIf(field -> {
      final boolean unnamed = StringUtils.isBlank(field.getBaseName());
      if (unnamed) {
        log.debug("Schema {} names no property, so it contributes no field to '{}'", schema, nameSchema);
      }
      return unnamed;
    });

    return fieldObjectArrayList;
  }

  /**
   * Resolves the property name to give a schema that is modelled as a single field. A schema node
   * carries no name of its own — {@code name} only exists on parameter-like nodes and
   * {@link ApiTool#getName} degrades to {@code null} for any other object node — so the name of the
   * schema being built is the only sensible fallback. A blank result means the schema names no
   * property at all.
   */
  private static String resolveFieldName(final JsonNode schema, final String nameSchema) {
    return StringUtils.defaultIfBlank(ApiTool.getName(schema), nameSchema);
  }

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private static List<SchemaFieldObject> processFieldObjectList(
      final String buildingSchema,
      final String fieldName, final String className, final JsonNode schema, final CommonSpecFile specFile,
      final Map<String, JsonNode> totalSchemas, final Map<String, SchemaObject> compositedSchemas,
      final Set<String> antiLoopList, final Path baseDir) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (TypeConstants.ARRAY.equalsIgnoreCase(ApiTool.getType(schema))) {
      fieldObjectArrayList.addAll(processArray(fieldName, className, schema, specFile, totalSchemas, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.hasAdditionalProperties(schema)) {
      fieldObjectArrayList.addAll(processMap(fieldName, schema, specFile, totalSchemas, compositedSchemas, antiLoopList, baseDir));
    } else if (ApiTool.hasPatternProperties(schema)) {
      fieldObjectArrayList.add(buildPatternPropertiesField(fieldName, schema, specFile));
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
        schemaObjectComposed = createComposedSchema(buildingSchema, composedSchemaName, schema, specFile,
                                                    totalSchemas, compositedSchemas, antiLoopList, baseDir);
      }

      if (describesNoProperty(schemaObjectComposed)) {
        warnAboutPropertylessComposition(fieldName, schemaObjectComposed);
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(new SchemaFieldObjectType(TypeConstants.OBJECT))
                                     .build());
      } else {
        compositedSchemas.put(composedSchemaName, schemaObjectComposed);
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(SchemaFieldObjectType.fromTypeList(schemaObjectComposed.getClassName(), schemaObjectComposed.getClassName()))
                                     .build());
      }
    }

    return fieldObjectArrayList;
  }

  /**
   * Tells whether a composed schema ended up with nothing to model, which happens when none of its
   * {@code allOf}/{@code anyOf}/{@code oneOf} members declares a property — as in
   * {@code anyOf: [{type: object}]}. Generating a class for it would yield an empty type that
   * silently drops every value, so the free-form {@code Object} is used in its place.
   */
  private static boolean describesNoProperty(final SchemaObject composedSchema) {
    return Objects.isNull(composedSchema) || CollectionUtils.isEmpty(composedSchema.getFieldObjectList());
  }

  /**
   * Reports a composition the contract cannot express as a class, so that the fallback is never a
   * silent surprise. It describes the contract rather than the generated type, because an
   * {@code allOf} member may still narrow the property afterwards.
   */
  private static void warnAboutPropertylessComposition(final String fieldName, final SchemaObject composedSchema) {
    final String combinator = Objects.nonNull(composedSchema) && StringUtils.isNotBlank(composedSchema.getSchemaCombinator())
                                  ? composedSchema.getSchemaCombinator()
                                  : "composition";
    log.warn("Property '{}' declares '{}' whose members define no property, so there is nothing to model and it falls back to a free-form value. "
             + "Give the members properties, or a $ref to a named schema, to get a typed model.", fieldName, combinator);
  }

  private static Consumer<Map.Entry<String, JsonNode>> processProperties(
      final String buildingSchema,
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
  private static List<SchemaFieldObject> processObjectProperty(
      final String buildingSchema,
      final Map<String, JsonNode> totalSchemas, final String fieldName, final JsonNode fieldBody, final Map<String, SchemaObject> compositedSchemas, final CommonSpecFile specFile,
      final JsonNode schema, final Set<String> antiLoopList, final Path baseDir) {
    final List<SchemaFieldObject> fieldObjectArrayList = new LinkedList<>();
    final var isRequired = ApiTool.checkIfRequired(fieldBody, fieldName);
    final SchemaFieldObject field;
    if (ApiTool.hasRef(fieldBody)) {
      final var typeName = MapperUtil.getRefSchemaName(fieldBody, fieldName);
      var refSchema = totalSchemas.get(MapperUtil.getRefSchemaKey(fieldBody));
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
        compositedSchemas.put(typeName, buildSchemaObject(totalSchemas, typeName, refSchema, antiLoopList, compositedSchemas, "", specFile, baseDir));
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
    applyMetadata(fieldObjectArrayList, fieldName, fieldBody);
    return fieldObjectArrayList;
  }

  private static void applyMetadata(final List<SchemaFieldObject> fields, final String fieldName, final JsonNode fieldBody) {
    final String description = ApiTool.getDescription(fieldBody);
    final String example = ApiTool.getExample(fieldBody);
    final boolean deprecated = ApiTool.isDeprecated(fieldBody);
    if (Objects.isNull(description) && Objects.isNull(example) && !deprecated) {
      return;
    }
    for (final var field : fields) {
      if (Objects.equals(field.getBaseName(), fieldName)) {
        if (Objects.nonNull(description)) {
          field.setDescription(description);
        }
        if (Objects.nonNull(example)) {
          field.setExample(example);
        }
        if (deprecated) {
          field.setDeprecated(true);
        }
      }
    }
  }

  private static Object getConst(final JsonNode fieldBody) {
    return ApiTool.hasConst(fieldBody) ? ApiTool.getConst(fieldBody) : null;
  }

  private static SchemaFieldObject processStringProperty(final String propertyName, final JsonNode schema, final CommonSpecFile specFile) {
    String resultingType;
    if (ApiTool.isDateTime(schema)) {
      resultingType = MapperUtil.getDateType(schema, specFile);
    } else if (ApiTool.isBinary(schema)) {
      resultingType = TypeConstants.MULTIPART_FILE;
    } else {
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

    if (!ApiTool.hasItems(schema) || ApiTool.getItems(schema).isBoolean()) {
      // No `items` schema, or `items: false` (JSON Schema 2020-12). A `prefixItems`
      // tuple becomes a List of its common element type when every position shares one
      // type, otherwise (mixed types, or nothing typable) the element type is Object.
      final boolean isTuple = ApiTool.hasPrefixItems(schema);
      fieldObjectArrayList.add(SchemaFieldObject
                                   .builder()
                                   .baseName(fieldName)
                                   .dataType(isTuple
                                                 ? SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, uniformPrefixItemType(schema, specFile))
                                                 : new SchemaFieldObjectType(TypeConstants.OBJECT))
                                   .build());
    } else {
      final var items = ApiTool.getItems(schema);
      final String primitiveItemType = ApiTool.hasRef(items) ? primitiveRefType(items, totalSchemas, specFile) : null;
      if (Objects.nonNull(primitiveItemType)) {
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, primitiveItemType))
                                     .build());
      } else if (ApiTool.hasRef(items)) {
        fieldObjectArrayList.add(
            processRef(fieldName, items, SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, MapperUtil.getSimpleType(items, specFile)), totalSchemas, compositedSchemas,
                       antiLoopList, specFile, baseDir));
      } else if (ApiTool.isComposed(items)) {
        final String composedSchemaName = StringUtils.defaultIfBlank(className, fieldName);
        SchemaObject schemaObjectComposed = compositedSchemas.get(composedSchemaName);
        if (Objects.isNull(schemaObjectComposed)) {
          schemaObjectComposed = createComposedSchema("", composedSchemaName, items, specFile,
                                                      totalSchemas, compositedSchemas, antiLoopList, baseDir);
        }

        if (describesNoProperty(schemaObjectComposed)) {
          warnAboutPropertylessComposition(fieldName, schemaObjectComposed);
          fieldObjectArrayList.add(SchemaFieldObject
                                       .builder()
                                       .baseName(fieldName)
                                       .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, TypeConstants.OBJECT))
                                       .build());
        } else {
          compositedSchemas.put(composedSchemaName, schemaObjectComposed);
          fieldObjectArrayList.add(SchemaFieldObject
                                       .builder()
                                       .baseName(fieldName)
                                       .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, schemaObjectComposed.getClassName()))
                                       .importClass(schemaObjectComposed.getClassName())
                                       .build());
        }
      } else if (ApiTool.hasProperties(items)) {
        final var itemsObject = buildSchemaObject(totalSchemas, className, items, antiLoopList, compositedSchemas, "", specFile, baseDir);
        compositedSchemas.put(className, itemsObject);
        fieldObjectArrayList.add(SchemaFieldObject
                                     .builder()
                                     .baseName(fieldName)
                                     .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, MapperUtil.getPojoName(fieldName, specFile)))
                                     .build());
      } else {
        final String itemType = ApiTool.isBinary(items) ? TypeConstants.MULTIPART_FILE : MapperUtil.getSimpleType(items, specFile);
        final SchemaFieldObject field = SchemaFieldObject
                                            .builder()
                                            .baseName(fieldName)
                                            .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.ARRAY, itemType))
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
    if (Objects.nonNull(addPropObj) && addPropObj.isBoolean()) {
      if (addPropObj.asBoolean()) {
        fieldObjectArrayList
            .add(SchemaFieldObject
                     .builder()
                     .baseName(fieldName)
                     .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.OBJECT))
                     .build());
      } else {
        return fieldObjectArrayList;
      }
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
                   .required(ApiTool.checkIfRequired(schema, fieldName) || ApiTool.hasConst(addPropObj))
                   .build());
    }

    return fieldObjectArrayList;
  }

  private static boolean isBasicType(final JsonNode value) {
    return !(ApiTool.isObject(value) || ApiTool.isArray(value) || ApiTool.isComposed(value)
             || ApiTool.hasAdditionalProperties(value));
  }

  private static SchemaObject createComposedSchema(
      final String buildingSchema,
      final String fieldName, final JsonNode schema, final CommonSpecFile specFile, final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();
    String schemaCombinatorType = "";
    if (ApiTool.isAllOf(schema)) {
      fieldObjectArrayList.addAll(
          processAllOf(totalSchemas, schema, specFile, compositedSchemas, antiLoopList, baseDir));
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
    // A nullable field (3.0 `nullable: true` or the 3.1 ["T","null"] idiom) must not be
    // marked required, otherwise the generated @NotNull would reject a legitimate null value.
    field.setRequired(ApiTool.hasRequired(schema) && ApiTool.checkIfRequired(schema, key) && !ApiTool.isNullable(schemaProperty));
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
      final JsonNode apNode = ApiTool.getAdditionalProperties(schemaProperty);
      if (Objects.nonNull(apNode) && apNode.isBoolean()) {
        if (apNode.asBoolean()) {
          final String typeObject = TypeConstants.OBJECT;
          field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, typeObject));
          field.setImportClass(getImportClass(typeObject));
        } // if boolean false -> do not set map type
      } else {
        final String typeObject = getMapTypeObject(schemaProperty, specFile);
        field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, typeObject));
        field.setImportClass(getImportClass(typeObject));
      }
    } else if (ApiTool.isObject(schemaProperty)) {
      String typeObject = ApiTool.getType(schemaProperty);
      if (ApiTool.hasRef(schemaProperty)) {
        typeObject = MapperUtil.getPojoNameFromRef(schema, specFile, null);
      }
      field.setImportClass(getImportClass(typeObject));
      field.getDataType().setDeepType(typeObject);
    }
  }

  private static String getMapTypeObject(final JsonNode schema, final CommonSpecFile specFile) {
    final String type;
    final JsonNode additionalProperties = ApiTool.getAdditionalProperties(schema);
    if (Objects.nonNull(additionalProperties) && additionalProperties.isBoolean()) {
      type = TypeConstants.OBJECT;
    } else {
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
      final Map<String, JsonNode> totalSchemas, final JsonNode allOfSchema, final CommonSpecFile specFile,
      final Map<String, SchemaObject> compositedSchemas, final Set<String> antiLoopList, final Path baseDir) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();
    final List<JsonNode> members = new ArrayList<>();

    for (JsonNode ref : ApiTool.getAllOf(allOfSchema)) {
      final Set<SchemaFieldObject> memberFields = new HashSet<>();
      if (ApiTool.hasRef(ref)) {
        final var schemaToProcess = totalSchemas.get(MapperUtil.getRefSchemaKey(ref));
        members.add(schemaToProcess);
        ApiTool.getProperties(schemaToProcess).forEachRemaining(processProperties("", totalSchemas, compositedSchemas, memberFields, specFile, schemaToProcess, antiLoopList,
                                                                                  baseDir));
      } else {
        members.add(ref);
        if (ApiTool.hasProperties(ref)) {
          ApiTool.getProperties(ref).forEachRemaining(processProperties("", totalSchemas, compositedSchemas, memberFields, specFile, ref, antiLoopList, baseDir));
        }
      }
      mergeAllOfMember(fieldObjectArrayList, memberFields);
    }
    markAllOfRequiredFields(fieldObjectArrayList, allOfSchema, members);
    return fieldObjectArrayList;
  }

  /**
   * Marks as required the properties that the {@code allOf} makes required. Combining schemas
   * requires nothing by itself: a property is required only when a {@code required} list names it,
   * and since a value must satisfy every member, that list may sit on any member (even one that does
   * not declare the property, such as {@code allOf: [{$ref: Base}, {required: [id]}]}) or on the
   * composing schema itself. As for any object, a nullable property is never marked required.
   */
  private static void markAllOfRequiredFields(final Set<SchemaFieldObject> fields, final JsonNode allOfSchema, final List<JsonNode> members) {
    final List<JsonNode> requiringSchemas = new ArrayList<>(members);
    requiringSchemas.add(allOfSchema);
    for (final var field : fields) {
      final String name = field.getBaseName();
      if (requiringSchemas.stream().anyMatch(schema -> ApiTool.checkIfRequired(schema, name))
          && members.stream().noneMatch(member -> isNullableProperty(member, name))) {
        field.setRequired(true);
      }
    }
  }

  private static boolean isNullableProperty(final JsonNode schema, final String propertyName) {
    return ApiTool.hasProperties(schema) && ApiTool.isNullable(ApiTool.getNode(schema, "properties").get(propertyName));
  }

  /**
   * Adds the properties of one {@code allOf} member to the ones already gathered. A value has to
   * satisfy every member, so when two members declare the same property the most specific
   * declaration is the accurate one: a member that types a property another member left free-form
   * (a paged wrapper narrowing its inherited {@code items} to the element type, say) replaces it.
   * Fields are keyed by name, so any other redeclaration keeps the one already gathered.
   */
  private static void mergeAllOfMember(final Set<SchemaFieldObject> gatheredFields, final Set<SchemaFieldObject> memberFields) {
    for (final var memberField : memberFields) {
      gatheredFields.stream()
                    .filter(gathered -> gathered.equals(memberField) && isFreeForm(gathered) && !isFreeForm(memberField))
                    .findFirst()
                    .ifPresent(gatheredFields::remove);
      gatheredFields.add(memberField);
    }
  }

  /**
   * Tells whether a field carries no information about its content beyond "some JSON value", which
   * is what the generator falls back to for a schema that declares nothing modellable.
   */
  private static boolean isFreeForm(final SchemaFieldObject field) {
    return FREE_FORM_TYPES.contains(Objects.toString(field.getDataType(), ""));
  }

  private static Set<SchemaFieldObject> processAnyOfOneOf(
      final String buildingSchema,
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

  /**
   * The Java type of a {@code $ref} to a named primitive schema, such as {@code State: {type: string}},
   * or {@code null} when it refers to anything else. On the wire such a value is the primitive itself,
   * so an array of them must be a list of that primitive: typing it after the named schema would make
   * each element an object wrapping the value, which cannot be read from, or written as, the plain
   * JSON value. Enums are left out, as they have a model of their own.
   */
  private static String primitiveRefType(final JsonNode refNode, final Map<String, JsonNode> totalSchemas, final CommonSpecFile specFile) {
    final JsonNode refSchema = totalSchemas.get(MapperUtil.getRefSchemaKey(refNode));
    final String type;
    if (Objects.isNull(refSchema) || ApiTool.isEnum(refSchema)) {
      type = null;
    } else if (ApiTool.isDateTime(refSchema)) {
      type = MapperUtil.getDateType(refSchema, specFile);
    } else if (ApiTool.isBinary(refSchema)) {
      type = TypeConstants.MULTIPART_FILE;
    } else if (ApiTool.isString(refSchema) || ApiTool.isNumber(refSchema) || ApiTool.isBoolean(refSchema)) {
      type = MapperUtil.getSimpleType(refSchema, specFile);
    } else {
      type = null;
    }
    return type;
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
    final Set<String> usedKeys = new HashSet<>();

    for (final var enumValue : enumValues) {
      String rawName = enumValue == null ? "" : enumValue;

      // Build a normalized, collision-free enum constant name
      final String keyBase = normalizeEnumKeyBase(rawName);
      String key;
      switch (dataType) {
        case TypeConstants.INTEGER:
          key = ensureUnique(usedKeys, "INTEGER_" + keyBase);
          enumValuesMap.put(key, enumValue);
          break;
        case TypeConstants.LONG:
          key = ensureUnique(usedKeys, "LONG_" + keyBase);
          enumValuesMap.put(key, enumValue + "l");
          break;
        case TypeConstants.DOUBLE:
          key = ensureUnique(usedKeys, "DOUBLE_" + keyBase);
          enumValuesMap.put(key, enumValue);
          break;
        case TypeConstants.FLOAT:
          key = ensureUnique(usedKeys, "FLOAT_" + keyBase);
          enumValuesMap.put(key, enumValue + "f");
          break;
        case TypeConstants.BIG_DECIMAL:
          key = ensureUnique(usedKeys, "BIG_DECIMAL_" + keyBase);
          enumValuesMap.put(key, "new BigDecimal(\"" + enumValue + "\")");
          break;
        case TypeConstants.STRING:
        default:
          // For string-based enums: uppercase and ensure it starts with a non-digit
          String candidate = StringUtils.upperCase(keyBase);
          if (candidate.isEmpty()) {
            candidate = "EMPTY";
          }
          if (Character.isDigit(candidate.charAt(0))) {
            candidate = "_" + candidate;
          }
          key = ensureUnique(usedKeys, candidate);
          enumValuesMap.put(key, '"' + enumValue + '"');
          break;
      }
    }

    if (enumValuesMap.isEmpty()) {
      throw new BadDefinedEnumException(name);
    }
    field.setEnumValues(enumValuesMap);
    // Enum fields bypass processObjectProperty's applyMetadata, so carry the schema's
    // description/example/deprecated here too for consistent @Schema annotations.
    field.setDescription(ApiTool.getDescription(value));
    field.setExample(ApiTool.getExample(value));
    field.setDeprecated(ApiTool.isDeprecated(value));
    return field;
  }

  /**
   * Normaliza una cadena para que pueda ser usada como parte de una constante Java:
   * - reemplaza puntos por _DOT_
   * - reemplaza cualquier caracter no alfanumérico por '_'
   * - colapsa guiones bajos múltiples
   * - recorta guiones bajos al inicio/fin
   */
  private static String normalizeEnumKeyBase(final String raw) {
    if (raw == null) {
      return "";
    }
    String s = raw.replace(".", "_DOT_");
    // Reemplaza cualquier caracter que no sea letra o dígito por '_'
    s = s.replaceAll("[^A-Za-z0-9]", "_");
    // Colapsa múltiples '_' en uno solo
    s = s.replaceAll("_+", "_");
    // Elimina '_' iniciales o finales
    s = s.replaceAll("^_+|_+$", "");
    // Si queda vacío, devolver placeholder
    if (s.isEmpty()) {
      return "EMPTY";
    }
    return s;
  }

  /**
   * Añade sufijo numérico si la clave ya existe para garantizar unicidad.
   */
  private static String ensureUnique(final Set<String> usedKeys, final String base) {
    String candidate = base;
    int idx = 1;
    while (usedKeys.contains(candidate)) {
      candidate = base + "_" + idx++;
    }
    usedKeys.add(candidate);
    return candidate;
  }

  private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !TypeConstants.NO_IMPORT_TYPE.contains(type) ? StringUtils.capitalize(type) : "";
  }

  private static String resolveArrayItemType(final JsonNode schema, final CommonSpecFile specFile) {
    final var items = ApiTool.getItems(schema);
    if (Objects.nonNull(items) && !items.isBoolean()) {
      return ApiTool.hasRef(items) ? MapperUtil.getPojoNameFromRef(items, specFile, null) : ApiTool.getType(items);
    }
    // `prefixItems` (tuple): common element type if uniform, else Object. `items: false` -> Object.
    return uniformPrefixItemType(schema, specFile);
  }

  private static String uniformPrefixItemType(final JsonNode schema, final CommonSpecFile specFile) {
    if (!ApiTool.hasPrefixItems(schema)) {
      return TypeConstants.OBJECT;
    }
    final JsonNode prefixItems = ApiTool.getPrefixItems(schema);
    if (Objects.isNull(prefixItems) || !prefixItems.isArray() || !prefixItems.elements().hasNext()) {
      return TypeConstants.OBJECT;
    }
    String common = null;
    for (final JsonNode item : prefixItems) {
      final String type;
      if (ApiTool.hasRef(item)) {
        type = MapperUtil.getPojoNameFromRef(item, specFile, null);
      } else if (ApiTool.hasType(item)) {
        type = MapperUtil.getSimpleType(item, specFile);
      } else {
        return TypeConstants.OBJECT;
      }
      if (Objects.isNull(common)) {
        common = type;
      } else if (!common.equals(type)) {
        return TypeConstants.OBJECT;
      }
    }
    return Objects.isNull(common) ? TypeConstants.OBJECT : common;
  }

  private static SchemaFieldObject buildPatternPropertiesField(
      final String fieldName, final JsonNode schema, final CommonSpecFile specFile) {
    // JSON Schema 2020-12 `patternProperties` maps regex keys to a value schema.
    // We model it as Map<String, ValueType> using the first declared pattern's value schema.
    final var patternProps = ApiTool.getPatternProperties(schema);
    final var valueSchemas = patternProps.elements();
    final JsonNode valueSchema = valueSchemas.hasNext() ? valueSchemas.next() : null;
    final String valueType;
    if (Objects.isNull(valueSchema)) {
      valueType = TypeConstants.OBJECT;
    } else if (ApiTool.hasRef(valueSchema)) {
      valueType = MapperUtil.getPojoNameFromRef(valueSchema, specFile, null);
    } else if (isBasicType(valueSchema) && ApiTool.hasType(valueSchema)) {
      valueType = MapperUtil.getSimpleType(valueSchema, specFile);
    } else {
      valueType = TypeConstants.OBJECT;
    }
    return SchemaFieldObject
               .builder()
               .baseName(StringUtils.defaultIfBlank(fieldName, ADDITIONAL_PROPERTIES))
               .dataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, valueType))
               .build();
  }


}
