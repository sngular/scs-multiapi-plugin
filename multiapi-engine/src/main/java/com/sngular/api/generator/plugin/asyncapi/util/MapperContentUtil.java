/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.asyncapi.exception.BadDefinedEnumException;
import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedSchemaException;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaFieldObjectProperties;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaObject;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;

public class MapperContentUtil {

  public static final String OBJECT = "object";

  public static final String ONE_OF = "oneOf";

  public static final String PROPERTIES = "properties";

  public static final String REF = "$ref";

  public static final String TYPE = "type";

  public static final String ALL_OF = "allOf";

  public static final String ANY_OF = "anyOf";

  private static final String ARRAY = "array";

  private static final String BIG_DECIMAL = "bigDecimal";

  private static final String MAP = "map";

  private static String schemaCombinatorType;

  private MapperContentUtil() {}

  public static List<SchemaObject> mapComponentToSchemaObject(
    final Map<String, JsonNode> totalSchemas, final String component, final JsonNode model,
    final String prefix, final String suffix, final String parentPackage) {
    final List<SchemaObject> schemasList = new ArrayList<>();
    if (Objects.nonNull(model)) {
      final Queue<String> modelToBuildList = new ConcurrentLinkedQueue<>();
      schemasList.add(buildSchemaObject(totalSchemas, component, model, prefix, suffix, modelToBuildList, parentPackage));
      while (!modelToBuildList.isEmpty()) {
        final var modelToBuild = modelToBuildList.remove();
        final var path = MapperUtil.splitName(modelToBuild);
        final var nexElement = buildSchemaObject(totalSchemas, modelToBuild, totalSchemas.get(getComponent(path)),
                                                 prefix, suffix, modelToBuildList, getParentName(path));
        if (schemasList.contains(nexElement)) {
          modelToBuildList.poll();
        } else {
          schemasList.add(nexElement);
        }
      }
    }
    return schemasList;
  }

  private static String getParentName(final String[] path) {
    final String parenName;
    if (path.length > 1) {
      parenName = path[path.length - 2];
    } else {
      parenName = "";
    }
    return parenName;
  }

  private static String getComponent(final String[] path) {
    final String componentName;
    if (path.length > 1) {
      componentName = path[path.length - 2] + "/" + path[path.length - 1];
    } else {
      componentName = path[0];
    }
    return componentName.toUpperCase();
  }

  private static SchemaObject buildSchemaObject(
    final Map<String, JsonNode> totalSchemas, final String component, final JsonNode model,
    final String prefix, final String suffix, final Collection<String> modelToBuildList, final String parentPackage) {

    final var listSchema = getFields(totalSchemas, model, true, prefix, suffix, modelToBuildList, parentPackage);
    final var splitPackage = MapperUtil.splitName(component);
    final String className = splitPackage[splitPackage.length - 1];
    return SchemaObject.builder()
                       .schemaName(WordUtils.capitalizeFully(className))
                       .className(MapperUtil.getPojoName(className, prefix, suffix))
                       .importList(getImportList(listSchema))
                       .schemaCombinator(StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
                       .fieldObjectList(listSchema)
                       .parentPackage(parentPackage.toLowerCase())
                       .build();
  }

  private static List<String> getImportList(final List<SchemaFieldObject> schemaListToImport) {
    final var listHashMap = new HashMap<String, List<String>>();
    final var importList = new HashSet<String>();

    for (SchemaFieldObject fieldObject : schemaListToImport) {
      importList.addAll(getTypeImports(fieldObject));
    }
    return new ArrayList<>(importList);
  }

  private static List<String> getTypeImports(final SchemaFieldObject fieldObject) {
    final List<String> importList = new ArrayList<>();
    if (Objects.nonNull(fieldObject.getDataTypeSimple())) {
      if (fieldObject.getDataTypeSimple().equals(ARRAY)) {
        importList.addAll(List.of("java.util.List", "java.util.ArrayList"));
      } else if (Objects.equals(fieldObject.getDataTypeSimple(), MAP)) {
        importList.addAll(List.of("java.util.Map", "java.util.HashMap"));
      } else if (fieldObject.getDataTypeSimple().equals(BIG_DECIMAL)
                 || Objects.nonNull(fieldObject.getDataType()) && fieldObject.getDataType().equals(BIG_DECIMAL)) {
        importList.add("java.math.BigDecimal");
      }
    }
    return importList;
  }

  private static List<SchemaFieldObject> getFields(
    final Map<String, JsonNode> totalSchemas, final JsonNode model, final boolean required, final String prefix,
    final String suffix, final Collection<String> modelToBuildList, final String parentPackage) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    schemaCombinatorType = null;
    if (ApiTool.hasType(model)) {
      if (OBJECT.equalsIgnoreCase(model.get(TYPE).textValue())) {
        fieldObjectArrayList.addAll(processFieldObject(totalSchemas, model, prefix, suffix, modelToBuildList, parentPackage));
      } else if (ARRAY.equalsIgnoreCase(model.get(TYPE).textValue())) {
        fieldObjectArrayList.add(processFieldObjectList(totalSchemas, "", model, required, prefix, suffix, modelToBuildList, parentPackage));
      } else if ("enum".equalsIgnoreCase(model.get(TYPE).textValue())) {
        fieldObjectArrayList.add(processFieldObjectList(totalSchemas, "", model, required, prefix, suffix, modelToBuildList, parentPackage));
      }
    } else if (ApiTool.hasRef(model)) {
      fieldObjectArrayList.addAll(processFieldObject(totalSchemas, totalSchemas.get(MapperUtil.buildKey(MapperUtil.splitName(ApiTool.getRefValue(model)))), prefix, suffix,
                                                     modelToBuildList, parentPackage));
    } else if (model.elements().hasNext()) {
      fieldObjectArrayList.addAll(processFieldObject(totalSchemas, model, prefix, suffix, modelToBuildList, parentPackage));
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processFieldObject(
    final Map<String, JsonNode> totalSchemas, final JsonNode model, final String prefix, final String suffix, final Collection<String> modelToBuildList,
    final String parentPackage) {
    final Set<String> requiredSet = new HashSet<>();
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    if (model.has("required")) {
      final JsonNode arrayNode = model.get("required");
      final Iterator<JsonNode> fields = arrayNode.iterator();
      fields.forEachRemaining(field -> requiredSet.add(field.textValue()));
    }
    final var properties = model.get(PROPERTIES);
    if (!(properties.has(ANY_OF) || properties.has(ALL_OF) || properties.has(ONE_OF))) {
      final var propertiesIt = model.get(PROPERTIES).fieldNames();
      while (propertiesIt.hasNext()) {
        final var property = propertiesIt.next();
        fieldObjectArrayList.add(processFieldObjectList(totalSchemas, property, model.get(PROPERTIES).path(property), requiredSet.contains(property), prefix, suffix,
                                                        modelToBuildList, parentPackage));
        if (model.get(PROPERTIES).path(property).has(REF)) {
          modelToBuildList.add(MapperUtil.getLongRefClass(model.get(PROPERTIES).path(property)));
        }
      }
    } else if (properties.has(ALL_OF)) {
      fieldObjectArrayList.addAll(processAllOfAnyOfOneOf(totalSchemas, properties.get(ALL_OF), true, prefix, suffix, modelToBuildList));
      schemaCombinatorType = ALL_OF;
    } else if (properties.has(ANY_OF)) {
      fieldObjectArrayList.addAll(processAllOfAnyOfOneOf(totalSchemas, properties.get(ANY_OF), false, prefix, suffix, modelToBuildList));
      schemaCombinatorType = ANY_OF;
    } else if (properties.has(ONE_OF)) {
      fieldObjectArrayList.addAll(processAllOfAnyOfOneOf(totalSchemas, properties.get(ONE_OF), false, prefix, suffix, modelToBuildList));
      schemaCombinatorType = ONE_OF;
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAllOfAnyOfOneOf(
    final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final boolean required, final String prefix, final String suffix,
    final Collection<String> modelToBuildList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    final var allOfIterator = schemaList.elements();

    allOfIterator.forEachRemaining(element -> fieldObjectArrayList.add(solveElement(totalSchemas, required, prefix, suffix, element, modelToBuildList)));
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject solveElement(
    final Map<String, JsonNode> totalSchemas, final boolean required, final String prefix, final String suffix,
    final JsonNode element, final Collection<String> modelToBuildList) {
    final SchemaFieldObject result;
    if (element.has(REF)) {
      final String schemaName = MapperUtil.getLongRefClass(element);
      final var schemaToProcess = totalSchemas.get(schemaName.toUpperCase());
      result = processFieldObjectList(totalSchemas, schemaName, schemaToProcess, required, prefix, suffix, modelToBuildList, null);
      result.setRequired(false);
    } else {
      result = processFieldObjectList(totalSchemas, "", element, required, prefix, suffix, modelToBuildList, null);
    }
    return result;
  }

  private static SchemaFieldObject processFieldObjectList(
    final Map<String, JsonNode> totalSchemas, final String propertyName, final JsonNode schema, final boolean required,
    final String prefix, final String suffix, final Collection<String> modelToBuildList, final String modelPackage) {
    final SchemaFieldObject fieldObject;
    final var name = schema.has("name") ? schema.get("name").textValue() : propertyName;
    if (ApiTool.hasType(schema)) {
      final var type = ApiTool.getType(schema);
      if (OBJECT.equalsIgnoreCase(type)) {
        fieldObject =
          SchemaFieldObject
            .builder()
            .baseName(name)
            .restrictions(new SchemaFieldObjectProperties())
            .dataType(MapperUtil.getSimpleType(schema, prefix, suffix))
            .build();
        setFieldType(fieldObject, schema, required, prefix, suffix);
        if (StringUtils.isNotEmpty(propertyName) && !totalSchemas.containsKey(propertyName)) {
          totalSchemas.put(createKey(modelPackage, propertyName.toUpperCase(), "/"), schema);
          modelToBuildList.add(createKey(modelPackage.toLowerCase(), propertyName, "."));
        }
      } else if (ApiTool.hasItems(schema)) {
        final var items = ApiTool.getItems(schema);
        final var arrayType = MapperUtil.getSimpleType(items, prefix, suffix);
        if (items.has(REF)) {
          final var longType = MapperUtil.getLongRefClass(items);
          modelToBuildList.add(longType);
        }
        fieldObject =
          SchemaFieldObject
            .builder()
            .baseName(name)
            .restrictions(new SchemaFieldObjectProperties())
            .dataType(arrayType)
            .dataTypeSimple(type)
            .importClass(getImportClass(arrayType))
            .build();
        handleItems(schema, modelToBuildList, fieldObject, required, items);
      } else if (ApiTool.isEnum(schema)) {
        fieldObject = processEnumField(name, required, schema, prefix, suffix);
      } else {
        fieldObject = SchemaFieldObject
          .builder()
          .baseName(name)
          .restrictions(new SchemaFieldObjectProperties())
          .dataType(MapperUtil.getSimpleType(schema, prefix, suffix))
          .dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix))
          .build();
        setFieldProperties(fieldObject, schema);
        fieldObject.setRequired(required);
      }
    } else if (ApiTool.hasRef(schema)) {
      fieldObject =
        SchemaFieldObject
          .builder()
          .baseName(name)
          .dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix))
          .restrictions(new SchemaFieldObjectProperties())
          .build();
      setFieldType(fieldObject, schema, required, prefix, suffix);
    } else {
      fieldObject = SchemaFieldObject
        .builder()
        .baseName(name)
        .dataType(MapperUtil.getSimpleType(schema, prefix, suffix))
        .dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix))
        .restrictions(new SchemaFieldObjectProperties())
        .build();
    }
    return fieldObject;
  }

  private static String createKey(final String modelPackage, final String className, final String separator) {
    return Objects.nonNull(modelPackage) ? modelPackage.toUpperCase() + separator + className : className;
  }

  private static void handleItems(
    final JsonNode schema, final Collection<String> modelToBuildList, final SchemaFieldObject fieldObject, final boolean required,
    final JsonNode items) {
    if (ApiTool.hasRef(items)) {
      modelToBuildList.add(MapperUtil.getLongRefClass(items));
    }
    final Iterator<Map.Entry<String, JsonNode>> iterator = schema.fields();
    Entry<String, JsonNode> current;
    while (iterator.hasNext()) {
      current = iterator.next();
      switch (current.getKey()) {
        case "maxItems":
          fieldObject.getRestrictions().setMaxItems(current.getValue().intValue());
          break;
        case "minItems":
          fieldObject.getRestrictions().setMinItems(current.getValue().intValue());
          break;
        case "uniqueItems":
          fieldObject.getRestrictions().setUniqueItems(current.getValue().booleanValue());
          break;
        default:
          break;
      }
    }
    fieldObject.setRequired(required);
  }

  private static void setFieldProperties(final SchemaFieldObject fieldObject, final JsonNode schema) {
    final Iterator<Map.Entry<String, JsonNode>> iterator = schema.fields();
    Entry<String, JsonNode> current;
    final SchemaFieldObjectProperties props = fieldObject.getRestrictions();
    while (iterator.hasNext()) {
      current = iterator.next();
      switch (current.getKey()) {
        case "minimum":
          props.setMinimum(current.getValue().asText());
          break;
        case "maximum":
          props.setMaximum(current.getValue().asText());
          break;
        case "exclusiveMinimum":
          props.setExclusiveMinimum(current.getValue().booleanValue());
          break;
        case "exclusiveMaximum":
          props.setExclusiveMaximum(current.getValue().booleanValue());
          break;
        case "maxItems":
          props.setMaxItems(current.getValue().intValue());
          break;
        case "maxLength":
          props.setMaxLength(current.getValue().intValue());
          break;
        case "minItems":
          props.setMinItems(current.getValue().intValue());
          break;
        case "minLength":
          props.setMinLength(current.getValue().intValue());
          break;
        case "pattern":
          props.setPattern(current.getValue().toString().replace("\"", ""));
          break;
        case "uniqueItems":
          props.setUniqueItems(current.getValue().booleanValue());
          break;
        case "multipleOf":
          props.setMultipleOf(current.getValue().asText());
          break;
        default:
          break;
      }
    }
  }

  private static void setFieldType(final SchemaFieldObject field, final JsonNode value, final boolean required, final String prefix, final String suffix) {
    field.setRequired(required);
    if (ApiTool.hasType(value)) {
      if (ARRAY.equalsIgnoreCase(ApiTool.getType(value))) {
        final var typeArray = MapperUtil.getTypeArray(value, prefix, suffix);
        field.setDataType(typeArray);
        field.setImportClass(getImportClass(typeArray));
      } else if (ApiTool.getType(value).equalsIgnoreCase(OBJECT)) {
        if (value.has("additionalProperties")) {
          final var typeMap = MapperUtil.getTypeMap(value, prefix, suffix);
          field.setDataTypeSimple(MAP);
          field.setDataType(typeMap);
          field.setImportClass(getImportClass(typeMap));
        } else {
          var typeObject = "";
          if (ApiTool.hasRef(value)) {
            typeObject = MapperUtil.getRef(value, prefix, suffix);
          } else {
            typeObject = MapperUtil.getPojoName(field.getBaseName(), prefix, suffix);
          }
          field.setImportClass(getImportClass(typeObject));
          field.setDataType(typeObject);
          field.setDataTypeSimple(typeObject);
        }
      } else {
        throw new NonSupportedSchemaException(value.toPrettyString());
      }
    }
  }

  private static SchemaFieldObject processEnumField(final String name, final boolean required, final JsonNode value, final String prefix, final String suffix) {
    final List<String> enumValues = new ArrayList<>();
    value.get("enum").elements().forEachRemaining(enumValue -> enumValues.add(enumValue.textValue()));

    if (enumValues.isEmpty()) {
      throw new BadDefinedEnumException(name);
    }

    return SchemaFieldObject
      .builder()
      .baseName(name)
      .dataTypeSimple("enum")
      .dataType(MapperUtil.getSimpleType(value, prefix, suffix))
      .required(required)
      .enumValues(enumValues)
      .restrictions(new SchemaFieldObjectProperties())
      .build();
  }

  private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !"String".equals(type) && !"Integer".equals(type) ? type : "";
  }

}
