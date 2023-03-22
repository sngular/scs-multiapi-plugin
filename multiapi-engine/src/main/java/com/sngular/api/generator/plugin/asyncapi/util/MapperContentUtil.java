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
import com.sngular.api.generator.plugin.asyncapi.model.SchemaObject;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {

  private static final String ARRAY = "array";

  private static final String MAP = "map";

  private static final String BIG_DECIMAL = "bigDecimal";

  public static final String $_REF = "$ref";

  public static final String TYPE = "type";

  public static final String ALL_OF = "allOf";

  public static final String ANY_OF = "anyOf";

  public static final String ONE_OF = "oneOf";

  private static String schemaCombinatorType;

  private MapperContentUtil() {}

  public static List<SchemaObject> mapComponentToSchemaObject(final Map<String, JsonNode> totalSchemas, final String modelPackage, final String component, final JsonNode model,
                                                              final String prefix, final String suffix, final String parentPackage) {
    final List<SchemaObject> schemasList = new ArrayList<>();
    if (Objects.nonNull(model)) {
      final Queue<String> modelToBuildList = new ConcurrentLinkedQueue<>();
      schemasList.add(buildSchemaObject(totalSchemas, modelPackage, component, model, prefix, suffix, modelToBuildList, parentPackage));
      while (!modelToBuildList.isEmpty()) {
        final var modelToBuild = modelToBuildList.remove();
        final var path = MapperUtil.splitName(modelToBuild);
        final var nexElement = buildSchemaObject(totalSchemas, modelPackage, modelToBuild, totalSchemas.get((path[path.length - 2] + "/" + path[path.length - 1]).toUpperCase()),
                                                 prefix, suffix, modelToBuildList, path.length >= 2 ? path[path.length - 2] : "");
        if (schemasList.contains(nexElement)) {
          modelToBuildList.remove();
        } else {
          schemasList.add(nexElement);
        }
      }
    }
    return schemasList;
  }

  private static SchemaObject buildSchemaObject(final Map<String, JsonNode> totalSchemas, final String modelPackage, final String component, final JsonNode model,
      final String prefix, final String suffix, final Collection<String> modelToBuildList, final String parentPackage) {

    final var listSchema = getFields(totalSchemas, model, true, prefix, suffix, modelToBuildList);
    final var splitPackage = MapperUtil.splitName(component);
    final String className = splitPackage[splitPackage.length - 1];
    return SchemaObject.builder()
                                .schemaName(StringUtils.capitalize(className))
                                .className(MapperUtil.getPojoName(className, prefix, suffix))
                                .importList(getImportList(listSchema, modelPackage))
                                .schemaCombinator(StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
                                .fieldObjectList(listSchema)
                                .parentPackage(parentPackage)
                                .build();
  }

  private static List<String> getImportList(final List<SchemaFieldObject> fieldObjectList, final String modelPackage) {
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
      } else if (Objects.nonNull(fieldObject.getDataTypeSimple()) && fieldObject.getDataTypeSimple().equals(BIG_DECIMAL)
                 || Objects.nonNull(fieldObject.getDataType()) && fieldObject.getDataType().equals(BIG_DECIMAL)) {
        listHashMap.computeIfAbsent(BIG_DECIMAL, key -> List.of("java.math.BigDecimal"));
      }
    }
  }

  private static List<SchemaFieldObject> getFields(final Map<String, JsonNode> totalSchemas, final JsonNode model, final boolean required, final String prefix,
      final String suffix, final Collection<String> modelToBuildList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    schemaCombinatorType = null;
    if (model.has(TYPE)) {
      if ("object".equalsIgnoreCase(model.get(TYPE).textValue())) {
        processFieldObject(totalSchemas, model, prefix, suffix, modelToBuildList, fieldObjectArrayList);
      } else if (ARRAY.equalsIgnoreCase(model.get(TYPE).textValue())) {
        fieldObjectArrayList.add(processFieldObjectList("", model, required, prefix, suffix, modelToBuildList));
      } else if ("enum".equalsIgnoreCase(model.get(TYPE).textValue())) {
        fieldObjectArrayList.add(processFieldObjectList("", model, required, prefix, suffix, modelToBuildList));
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
      model.get("required").fieldNames().forEachRemaining(requiredSet::add);
    }
    final var properties = model.get("properties");
    if (!(properties.has(ANY_OF) || properties.has(ALL_OF) || properties.has(ONE_OF))) {
      final var propertiesIt = model.get("properties").fieldNames();
      while (propertiesIt.hasNext()) {
        final var property = propertiesIt.next();
        fieldObjectArrayList.add(
            processFieldObjectList(property, model.get("properties").path(property), requiredSet.contains(property), prefix, suffix, modelToBuildList));
        if (model.get("properties").path(property).has($_REF)) {
          modelToBuildList.add(MapperUtil.getLongRefClass(model.get("properties").path(property)));
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

  private static void extractFieldsComplexObject(final ArrayList<SchemaFieldObject> fieldObjectArrayList, final Iterator<Entry<String, JsonNode>> fieldsIt,
      final Collection<String> modelToBuildList, final String prefix, final String suffix) {
    while (fieldsIt.hasNext()) {
      final var field = fieldsIt.next();
      final var fieldName = field.getKey();
      final var fieldBody = field.getValue();
      if (fieldBody.has($_REF)) {
        String fieldType = extractTypeFromBody(fieldBody);
        modelToBuildList.add(fieldType);
        final var splitPackage = MapperUtil.splitName(fieldType);
        fieldType = splitPackage[splitPackage.length - 1];
        fieldObjectArrayList.add(
            SchemaFieldObject.builder()
                           .baseName(fieldName)
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
    String bodyType = fieldBody.get($_REF).asText();
    if (bodyType.contains("#")) {
      final String[] path = MapperUtil.splitName(bodyType);
      bodyType = path[path.length - 2] + "." + StringUtils.capitalize(path[path.length - 1]);
    }
    return bodyType;
  }

  private static List<SchemaFieldObject> processAllOfAnyOfOneOf(final Map<String, JsonNode> totalSchemas, final JsonNode schemaList, final boolean required, final String prefix,
      final String suffix,
      final Collection<String> modelToBuildList) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    final var allOfIterator = schemaList.elements();

    allOfIterator.forEachRemaining(element -> fieldObjectArrayList.add(solveElement(totalSchemas, required, prefix, suffix, element, modelToBuildList)));
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject solveElement(final Map<String, JsonNode> totalSchemas, final boolean required, final String prefix, final String suffix,
      final JsonNode element, final Collection<String> modelToBuildList) {
    final SchemaFieldObject result;
    if (element.has($_REF)) {
      final String schemaName = MapperUtil.getLongRefClass(element);
      final var schemaToProcess = totalSchemas.get(schemaName.toUpperCase());
      result = processFieldObjectList(schemaName, schemaToProcess, required, prefix, suffix, modelToBuildList);
      result.setRequired(false);
    } else {
      result = processFieldObjectList("", element, required, prefix, suffix, modelToBuildList);
    }
    return result;
  }

  private static SchemaFieldObject processFieldObjectList(final String propertyName, final JsonNode schema, final boolean required, final String prefix, final String suffix,
      final Collection<String> modelToBuildList) {
    final SchemaFieldObject fieldObject;
    final var name = schema.has("name") ? schema.get("name").textValue() : propertyName;
    if (schema.has(TYPE)) {
      final var type = schema.get(TYPE).textValue();
      if ("object".equalsIgnoreCase(type)) {
        fieldObject = SchemaFieldObject.builder().baseName(name).dataType(MapperUtil.getSimpleType(schema, prefix, suffix)).build();
        setFieldType(fieldObject, schema, required, prefix, suffix);
      } else if (schema.has("items")) {
        final var items = schema.get("items");
        final var arrayType = MapperUtil.getSimpleType(items, prefix, suffix);
        String parentPackage = null;
        if (items.has($_REF)) {
          final var longType = MapperUtil.getLongRefClass(items);
          final String[] path = MapperUtil.splitName(longType);
          parentPackage = path.length > 1 ? path[path.length - 2] : "";
          modelToBuildList.add(longType);
        }
        fieldObject =
          SchemaFieldObject
            .builder()
            .baseName(name)
            .dataType(arrayType)
            .dataTypeSimple(type)
            .importClass(getImportClass(arrayType))
            .parentPackage(parentPackage)
            .build();
      } else if (schema.has("enum")) {
        fieldObject = processEnumField(name, required, schema, prefix, suffix);
      } else {
        fieldObject = SchemaFieldObject
          .builder().baseName(name).dataType(MapperUtil.getSimpleType(schema, prefix, suffix)).dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix)).build();
      }
    } else if (schema.has($_REF)) {
      final String[] path = MapperUtil.splitName(schema.get($_REF).textValue());
      final String refSchemaName = MapperUtil.getRef(schema, prefix, suffix);
      fieldObject =
        SchemaFieldObject
          .builder()
          .baseName(refSchemaName)
          .dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix))
            .parentPackage(path[path.length - 2])
          .build();
      setFieldType(fieldObject, schema, required, prefix, suffix);
    } else {
      fieldObject = SchemaFieldObject
        .builder().baseName(name).dataType(MapperUtil.getSimpleType(schema, prefix, suffix)).dataTypeSimple(MapperUtil.getSimpleType(schema, prefix, suffix)).build();
    }
    return fieldObject;
  }

  private static void setFieldType(final SchemaFieldObject field, final JsonNode value, final boolean required, final String prefix, final String suffix) {
    field.setRequired(required);
    if (value.has(TYPE)) {
      if (ARRAY.equalsIgnoreCase(value.get(TYPE).textValue())) {
        final var typeArray = MapperUtil.getTypeArray(value, prefix, suffix);
        field.setDataType(typeArray);
        field.setImportClass(getImportClass(typeArray));
      } else if (value.get(TYPE).textValue().equalsIgnoreCase("object")) {
        if (value.has("additionalProperties")) {
          final var typeMap = MapperUtil.getTypeMap(value, prefix, suffix);
          field.setDataTypeSimple(MAP);
          field.setDataType(typeMap);
          field.setImportClass(getImportClass(typeMap));
        } else {
          var typeObject = "";
          if (value.has($_REF)) {
            typeObject = MapperUtil.getRef(value, prefix, suffix);
          }
          field.setImportClass(getImportClass(typeObject));
          field.setDataType(typeObject);
          final String[] path = MapperUtil.splitName(value.get($_REF).textValue());
          field.setParentPackage(path[path.length - 2]);
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
      .build();
  }

  private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !"String".equals(type) && !"Integer".equals(type) ? type : "";
  }

}
