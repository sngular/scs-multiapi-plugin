/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.databind.JsonNode;
import net.coru.api.generator.plugin.asyncapi.exception.BadDefinedEnumException;
import net.coru.api.generator.plugin.asyncapi.exception.NonSupportedSchemaException;
import net.coru.api.generator.plugin.asyncapi.model.SchemaFieldObject;
import net.coru.api.generator.plugin.asyncapi.model.SchemaObject;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {

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

  public static List<SchemaObject> mapComponentToSchemaObject(final Map<String, JsonNode> totalSchemas, final String modelPackage, final String component, final JsonNode model,
      final String prefix, final String suffix) {
    final List<SchemaObject> schemasList = new ArrayList<>();
    if (model.has("payload")) {

      schemasList.add(SchemaObject.builder()
                                  .schemaName(component)
                                  .className(component)
                                  .importList(List.of(modelPackage + "." + component + "Payload"))
                                  .schemaCombinator("")
                                  .fieldObjectList(List.of(
                                    SchemaFieldObject
                                      .builder()
                                      .baseName("payload")
                                      .dataType(component + "Payload")
                                      .dataTypeSimple("")
                                      .importClass(component + "Payload")
                                      .required(true)
                                      .build()))
                                  .build());

      final var listSchema = getFields(totalSchemas, modelPackage, model, true, prefix, suffix);

      schemasList.add(SchemaObject.builder()
                                  .schemaName(component + "Payload")
                                  .className(component + "Payload")
                                  .importList(getImportList(listSchema, modelPackage))
                                  .schemaCombinator(StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
                                  .fieldObjectList(listSchema)
                                  .build());
    } else {

      final var listSchema = getFields(totalSchemas, modelPackage, model, true, prefix, suffix);

      schemasList.add(SchemaObject.builder()
                                  .schemaName(component)
                                  .className(component)
                                  .importList(getImportList(listSchema, modelPackage))
                                  .schemaCombinator(StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
                                  .fieldObjectList(listSchema)
                                  .build());
    }

    return schemasList;
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
      if (fieldObject.getDataTypeSimple().equals(ARRAY)) {
        listHashMap.computeIfAbsent(ARRAY, key -> List.of("java.util.List", "java.util.ArrayList"));
      } else if (Objects.equals(fieldObject.getDataTypeSimple(), MAP)) {
        listHashMap.computeIfAbsent(MAP, key -> List.of("java.util.Map", "java.util.HashMap"));
      } else if (Objects.nonNull(fieldObject.getDataTypeSimple()) && fieldObject.getDataTypeSimple().equals(BIG_DECIMAL)
                 || Objects.nonNull(fieldObject.getDataType()) && fieldObject.getDataType().equals(BIG_DECIMAL)) {
        listHashMap.computeIfAbsent(BIG_DECIMAL, key -> List.of("java.math.BigDecimal"));
      }
    }
  }

  private static List<SchemaFieldObject> getFields(final Map<String, JsonNode> totalSchemas, final String modelPackage, final JsonNode model, final boolean required, final String prefix,
      final String suffix) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    schemaCombinatorType = null;
    if (model.has("type")) {
      if ("object".equalsIgnoreCase(model.get("type").textValue())) {
        final Set<String> requiredSet = new HashSet<>();
        if (model.has("required")) {
          model.get("required").fieldNames().forEachRemaining(requiredSet::add);
        }
        final var properties = model.get("properties");
        if (!(properties.has("anyOf") || properties.has("allOf") || properties.has("oneOf"))) {
          final var propertiesIt = model.get("properties").fieldNames();
          while (propertiesIt.hasNext()) {
            final var property = propertiesIt.next();
            final var type = properties.get(property).get("type").asText();
            fieldObjectArrayList.add(
                  SchemaFieldObject.builder()
                               .baseName(property)
                               .dataType(type)
                               .dataTypeSimple("")
                               .required(requiredSet.contains(property))
                               .build());

          }
        } else if (properties.has("allOf")) {
          fieldObjectArrayList.addAll(processAllOf(totalSchemas, properties.get("allOf"), prefix, suffix));
          schemaCombinatorType = "allOf";
        } else if (properties.has("anyOf")) {
          fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, properties.get("anyOf"), prefix, suffix));
          schemaCombinatorType = "anyOf";
        } else if (properties.has("oneOf")) {
          fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, properties.get("oneOf"), prefix, suffix));
          schemaCombinatorType = "oneOf";
        }
      } else if ("array".equalsIgnoreCase(model.get("type").textValue())) {
        fieldObjectArrayList.add(processFieldObjectList(model, required, prefix, suffix));
      } else if ("enum".equalsIgnoreCase(model.get("type").textValue())) {
        fieldObjectArrayList.add(processFieldObjectList(model, required, prefix, suffix));
      }
    } else if (model.elements().hasNext()) {
      final var fieldsIt = model.fields();
      extractFieldsComplexPayload(fieldObjectArrayList, fieldsIt);
    }
    return fieldObjectArrayList;
  }

  private static void extractFieldsComplexPayload(final ArrayList<SchemaFieldObject> fieldObjectArrayList, final Iterator<Entry<String, JsonNode>> fieldsIt) {
    while (fieldsIt.hasNext()) {
      final var field = fieldsIt.next();
      final var fieldName = field.getKey();
      final var fieldBody = field.getValue();
      if (fieldBody.has("$ref")) {
        final String fieldType = extractTypeFromBody(fieldBody);
        fieldObjectArrayList.add(
          SchemaFieldObject.builder()
                           .baseName(fieldName)
                           .dataType(fieldType)
                           .importClass(fieldType)
                           .required(false)
                           .build());
      } else if (fieldBody.elements().hasNext()) {
        final var fieldObjectsIt = fieldBody.fields();
        extractFieldsComplexPayload(fieldObjectArrayList, fieldObjectsIt);
      }
    }
  }

  private static String extractTypeFromBody(final JsonNode fieldBody) {
    String bodyType = fieldBody.get("$ref").asText();
    if (bodyType.contains("#")) {
      bodyType = bodyType.substring(bodyType.lastIndexOf("/") + 1);
    }
    return bodyType;
  }

  private static List<SchemaFieldObject> processAllOf(final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final String prefix,
      final String suffix) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    final var allOfIterator = schemaList.elements();
    allOfIterator.forEachRemaining(element -> fieldObjectArrayList.add(solveElement(totalSchemas, true, prefix, suffix, element)));
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAnyOfOneOf(final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final String prefix, final String suffix) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    final var allOfIterator = schemaList.elements();

    allOfIterator.forEachRemaining(element -> fieldObjectArrayList.add(solveElement(totalSchemas, false, prefix, suffix, element)));
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject solveElement(final Map<String, JsonNode> totalSchemas, final boolean required, final String prefix, final String suffix,
    final JsonNode element) {
    SchemaFieldObject result;
    if (element.has("$ref")) {
      final String[] pathObjectRef = element.get("$ref").textValue().split("/");
      final String schemaName = pathObjectRef[pathObjectRef.length - 1];
      final var schemaToProcess = totalSchemas.get(schemaName);
      result = processFieldObjectList(schemaToProcess, required, prefix, suffix);
        result.setRequired(false);
    } else {
      result = processFieldObjectList(element, required, prefix, suffix);
    }
    return result;
  }

  private static SchemaFieldObject processFieldObjectList(final JsonNode schema, final boolean required, final String prefix, final String suffix) {
    final SchemaFieldObject fieldObject;
    final var name = schema.has("name") ? schema.get("name").textValue() : "";
    final var type = schema.get("type").textValue();
    if (ARRAY.equalsIgnoreCase(type)) {
      if (schema.has("items")) {
        var items = schema.get("items");
        final var arrayType = MapperUtil.getSimpleType(items, prefix, suffix);
        String baseName = name;
        if (items.has("$ref")) {
          baseName = getRef(items, prefix, suffix);
        }
        fieldObject =
          SchemaFieldObject
            .builder()
            .baseName(baseName)
            .dataType(MapperUtil.getTypeArray(schema, prefix, suffix))
            .dataTypeSimple(arrayType)
            .importClass(getImportClass(arrayType))
            .build();
      } else {
        throw new NonSupportedSchemaException(schema.toPrettyString());
      }
    } else if (schema.has("$ref")) {
      final String refSchemaName = getRef(schema, prefix, suffix);
      fieldObject =
        SchemaFieldObject
          .builder()
          .baseName(refSchemaName)
          .dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix))
          .build();
      setFieldType(fieldObject, schema, required, prefix, suffix);
    } else if (schema.has("items")) {
      fieldObject = processEnumField(name, required, schema, prefix, suffix);
    } else if ("object".equalsIgnoreCase(type)) {
      fieldObject = SchemaFieldObject.builder().baseName(name).dataType(MapperUtil.getSimpleType(schema, prefix, suffix)).build();
      setFieldType(fieldObject, schema, required, prefix, suffix);
    } else {
      throw new NonSupportedSchemaException(schema.toPrettyString());
    }
    return fieldObject;
  }

  private static void setFieldType(final SchemaFieldObject field, final JsonNode value, final boolean required, final String prefix, final String suffix) {
    field.setRequired(required);
    if (value.has("type")) {
      if (value.get("type").textValue().equalsIgnoreCase("array")) {
        final var typeArray = MapperUtil.getTypeArray(value, prefix, suffix);
        field.setDataType(typeArray);
        field.setImportClass(getImportClass(typeArray));
      } else if (value.get("type").textValue().equalsIgnoreCase("object")) {
        if (value.has("additionalProperties")) {
          final var typeMap = MapperUtil.getTypeMap(value, prefix, suffix);
          field.setDataTypeSimple(MAP);
          field.setDataType(typeMap);
          field.setImportClass(getImportClass(typeMap));
        } else {
          var typeObject = "";
          if (value.has("$ref")) {
            typeObject = getRef(value, prefix, suffix);
          }
          field.setImportClass(getImportClass(typeObject));
          field.setDataType(typeObject);
        }
      } else {
        throw new NonSupportedSchemaException(value.toPrettyString());
      }
    }
  }

  private static String getRef(final JsonNode schema, final String prefix, final String suffix) {
    final String[] pathObjectRef = schema.get("$ref").textValue().split("/");
    return MapperUtil.getPojoName(pathObjectRef[pathObjectRef.length - 1], prefix, suffix);
  }

  private static SchemaFieldObject processEnumField(final String name, final boolean required, final JsonNode value, final String prefix, final String suffix) {
    List<String> enumValues = new ArrayList<>();
    value.elements().forEachRemaining(enumValue -> enumValues.add(enumValue.textValue()));

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
      .build();
  }

 private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !"String".equals(type) && !"Integer".equals(type) ? type : "";
  }

}
