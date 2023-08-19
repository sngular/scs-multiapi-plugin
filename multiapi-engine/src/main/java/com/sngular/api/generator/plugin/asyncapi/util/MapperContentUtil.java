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

  private static String getParentName(String[] path) {
    final String parenName;
    if (path.length > 1) {
      parenName = path[path.length - 2];
    } else {
      parenName = "";
    }
    return parenName;
  }

  private static String getComponent(String[] path) {
    final String componentName;
    if (path.length > 1) {
      componentName = (path[path.length - 2] + "/" + path[path.length - 1]);
    } else {
      componentName = path[0];
    }
    return componentName.toUpperCase();
  }

  private static SchemaObject buildSchemaObject(
      final Map<String, JsonNode> totalSchemas, final String component, final JsonNode model,
      final String prefix, final String suffix, final Collection<String> modelToBuildList, final String parentPackage) {

    final var listSchema = getFields(totalSchemas, model, true, prefix, suffix, modelToBuildList);
    final var splitPackage = MapperUtil.splitName(component);
    final String className = splitPackage[splitPackage.length - 1];
    return SchemaObject.builder()
                       .schemaName(StringUtils.capitalize(className))
                       .className(MapperUtil.getPojoName(className, prefix, suffix))
                       .importList(getImportList(listSchema))
                       .schemaCombinator(StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
                       .fieldObjectList(listSchema)
                       .parentPackage(parentPackage)
                       .build();
  }

  private static List<String> getImportList(final List<SchemaFieldObject> fieldObjectList) {
    final var listHashMap = new HashMap<String, List<String>>();
    final var importList = new ArrayList<String>();

    for (SchemaFieldObject fieldObject : fieldObjectList) {
      getTypeImports(listHashMap, fieldObject);
    }
    if (!listHashMap.isEmpty()) {
      listHashMap.forEach((key, value) -> importList.addAll(value));
    }
    return importList;
  }

  private static void getTypeImports(final HashMap<String, List<String>> listHashMap, final SchemaFieldObject fieldObject) {
    if (Objects.nonNull(fieldObject.getDataTypeSimple())) {
      if (fieldObject.getDataTypeSimple().equals(ARRAY)) {
        listHashMap.computeIfAbsent(ARRAY, key -> List.of("java.util.List", "java.util.ArrayList"));
      } else if (Objects.equals(fieldObject.getDataTypeSimple(), MAP)) {
        listHashMap.computeIfAbsent(MAP, key -> List.of("java.util.Map", "java.util.HashMap"));
      } else if (fieldObject.getDataTypeSimple().equals(BIG_DECIMAL)
                 || Objects.nonNull(fieldObject.getDataType()) && fieldObject.getDataType().equals(BIG_DECIMAL)) {
        listHashMap.computeIfAbsent(BIG_DECIMAL, key -> List.of("java.math.BigDecimal"));
      }
    }
  }

  private static List<SchemaFieldObject> getFields(
      final Map<String, JsonNode> totalSchemas, final JsonNode model, final boolean required, final String prefix,
      final String suffix, final Collection<String> modelToBuildList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    schemaCombinatorType = null;
    if (ApiTool.hasType(model)) {
      if (OBJECT.equalsIgnoreCase(model.get(TYPE).textValue())) {
        processFieldObject(totalSchemas, model, prefix, suffix, modelToBuildList, fieldObjectArrayList);
      } else if (ARRAY.equalsIgnoreCase(model.get(TYPE).textValue())) {
        fieldObjectArrayList.add(processFieldObjectList(totalSchemas, "", model, required, prefix, suffix, modelToBuildList));
      } else if ("enum".equalsIgnoreCase(model.get(TYPE).textValue())) {
        fieldObjectArrayList.add(processFieldObjectList(totalSchemas, "", model, required, prefix, suffix, modelToBuildList));
      }
    } else if (model.elements().hasNext()) {
      final var fieldsIt = model.fields();
      extractFieldsComplexObject(fieldObjectArrayList, fieldsIt, modelToBuildList, prefix, suffix);
    }
    return fieldObjectArrayList;
  }

  private static void processFieldObject(
      final Map<String, JsonNode> totalSchemas, final JsonNode model, final String prefix, final String suffix, final Collection<String> modelToBuildList,
      final ArrayList<SchemaFieldObject> fieldObjectArrayList) {
    final Set<String> requiredSet = new HashSet<>();
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
        fieldObjectArrayList.add(
            processFieldObjectList(totalSchemas, property, model.get(PROPERTIES).path(property), requiredSet.contains(property), prefix, suffix, modelToBuildList));
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
  }

  private static void extractFieldsComplexObject(
      final ArrayList<SchemaFieldObject> fieldObjectArrayList, final Iterator<Entry<String, JsonNode>> fieldsIt,
      final Collection<String> modelToBuildList, final String prefix, final String suffix) {
    while (fieldsIt.hasNext()) {
      final var field = fieldsIt.next();
      final var fieldName = field.getKey();
      final var fieldBody = field.getValue();
      if (fieldBody.has(REF)) {
        String fieldType = extractTypeFromBody(fieldBody);
        modelToBuildList.add(fieldType);
        final var splitPackage = MapperUtil.splitName(fieldType);
        fieldType = splitPackage[splitPackage.length - 1];
        fieldObjectArrayList.add(
            SchemaFieldObject.builder()
                             .baseName(fieldName)
                             .restrictions(new SchemaFieldObjectProperties())
                             .dataType(MapperUtil.getPojoName(fieldType, prefix, suffix))
                             .dataTypeSimple(MapperUtil.getPojoName(fieldType, prefix, suffix))
                             .importClass(MapperUtil.getPojoName(fieldType, prefix, suffix))
                             .required(false)
                             .parentPackage(splitPackage[splitPackage.length - 2])
                             .build());
      } else if (fieldBody.elements().hasNext()) {
        final var fieldObjectsIt = fieldBody.fields();
        extractFieldsComplexObject(fieldObjectArrayList, fieldObjectsIt, modelToBuildList, prefix, suffix);
      }
    }
  }

  private static String extractTypeFromBody(final JsonNode fieldBody) {
    String bodyType = fieldBody.get(REF).asText();
    if (bodyType.contains("#")) {
      final String[] path = MapperUtil.splitName(bodyType);
      bodyType = path[path.length - 2] + "." + StringUtils.capitalize(path[path.length - 1]);
    }
    return bodyType;
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
      result = processFieldObjectList(totalSchemas, schemaName, schemaToProcess, required, prefix, suffix, modelToBuildList);
      result.setRequired(false);
    } else {
      result = processFieldObjectList(totalSchemas, "", element, required, prefix, suffix, modelToBuildList);
    }
    return result;
  }

  private static SchemaFieldObject processFieldObjectList(
      final Map<String, JsonNode> totalSchemas, final String propertyName, final JsonNode schema, final boolean required, final String prefix, final String suffix,
      final Collection<String> modelToBuildList) {
    final SchemaFieldObject fieldObject;
    final var name = schema.has("name") ? schema.get("name").textValue() : propertyName;
    if (schema.has("type")) {
      final var type = schema.get("type").textValue();
      if (OBJECT.equalsIgnoreCase(type)) {
        fieldObject = SchemaFieldObject.builder().restrictions(new SchemaFieldObjectProperties()).baseName(name).dataType(MapperUtil.getSimpleType(schema, prefix, suffix)).build();
        setFieldType(fieldObject, schema, required, prefix, suffix);
        if (StringUtils.isNotEmpty(propertyName) && !totalSchemas.containsKey(propertyName)) {
          totalSchemas.put(propertyName.toUpperCase(), schema);
          modelToBuildList.add(propertyName);
        }
      } else if (schema.has("items")) {
        final var items = schema.get("items");
        final var arrayType = MapperUtil.getSimpleType(items, prefix, suffix);
        String parentPackage = null;
        if (items.has(REF)) {
          final var longType = MapperUtil.getLongRefClass(items);
          final String[] path = MapperUtil.splitName(longType);
          parentPackage = path.length > 1 ? path[path.length - 2] : "";
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
                .parentPackage(parentPackage)
                .build();
        handleItems(schema, modelToBuildList, fieldObject, required, items);
      } else if (schema.has("enum")) {
        fieldObject = processEnumField(name, required, schema, prefix, suffix);
      } else {
        fieldObject = SchemaFieldObject
                          .builder()
                          .restrictions(new SchemaFieldObjectProperties()).baseName(name).dataType(MapperUtil.getSimpleType(schema, prefix, suffix))
                          .dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix))
                          .build();
        setFieldProperties(fieldObject, schema);
        fieldObject.setRequired(required);
      }
    } else if (schema.has(REF)) {
      final String[] path = MapperUtil.splitName(schema.get(REF).textValue());
      final String refSchemaName = MapperUtil.getRef(schema, prefix, suffix);
      fieldObject =
          SchemaFieldObject
              .builder()
              .baseName(refSchemaName)
              .dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix))
              .restrictions(new SchemaFieldObjectProperties())
              .parentPackage(path[path.length - 2])
              .build();
      setFieldType(fieldObject, schema, required, prefix, suffix);
    } else {
      fieldObject = SchemaFieldObject
                        .builder().baseName(name).dataType(MapperUtil.getSimpleType(schema, prefix, suffix)).dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix))
                        .restrictions(new SchemaFieldObjectProperties())
                        .build();
    }
    return fieldObject;
  }

  private static void handleItems(final JsonNode schema, final Collection<String> modelToBuildList, final SchemaFieldObject fieldObject, final boolean required,
      final JsonNode items) {
    if (items.has("$ref")) {
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
          if (ApiTool.hasRef(value)) {
            final String[] path = MapperUtil.splitName(ApiTool.getRefValue(value));
            field.setParentPackage(path[path.length - 2]);
          }
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
