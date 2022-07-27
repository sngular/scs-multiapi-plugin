/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import net.coru.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import net.coru.api.generator.plugin.openapi.model.SchemaFieldObject;
import net.coru.api.generator.plugin.openapi.model.SchemaObject;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import org.apache.commons.collections4.CollectionUtils;
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

  public static SchemaObject mapComponentToSchemaObject(
      final Map<String, Schema> totalSchemas, final Schema<?> schema, final String nameSchema, final FileSpec fileSpec,
      final String modelPackage) {
    final var listSchema = getFields(totalSchemas, schema, fileSpec);

    return SchemaObject.builder()
                       .schemaName(schema.getName())
                       .className(MapperUtil.getPojoName(nameSchema, fileSpec))
                       .importList(getImportList(listSchema, modelPackage))
                       .schemaCombinator(StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
                       .fieldObjectList(listSchema)
                       .build();
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
      } else if (Objects.nonNull(fieldObject.getDataType()) && fieldObject.getDataType().equals(BIG_DECIMAL)) {
        listHashMap.computeIfAbsent(BIG_DECIMAL, key -> List.of("java.math.BigDecimal"));
      }
    }
  }

  private static List<SchemaFieldObject> getFields(final Map<String, Schema> totalSchemas, final Schema<?> schema, final FileSpec fileSpec) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (Objects.nonNull(schema.getProperties())) {
      fieldObjectArrayList.addAll(processFieldObjectList(schema, fileSpec));
    } else if (Objects.nonNull(schema.getAllOf())) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, schema.getAllOf(), fileSpec));
      schemaCombinatorType = "allOf";
    } else if (Objects.nonNull(schema.getAnyOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getAnyOf(), fileSpec));
      schemaCombinatorType = "anyOf";
    } else if (Objects.nonNull(schema.getOneOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getOneOf(), fileSpec));
      schemaCombinatorType = "oneOf";
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAllOf(final Map<String, Schema> totalSchemas, final List<Schema> schemaList, final FileSpec fileSpec) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (Schema<?> ref : schemaList) {
      if (Objects.nonNull(ref.get$ref())) {
        final String[] pathObjectRef = ref.get$ref().split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        fieldObjectArrayList.addAll(processFieldObjectList(schemaToProcess, fileSpec));
        for (var fieldObject : fieldObjectArrayList) {
          fieldObject.setRequired(true);
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAnyOfOneOf(final Map<String, Schema> totalSchemas, final List<Schema> schemaList, final FileSpec fileSpec) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (Schema<?> internalSchema : schemaList) {
      if (Objects.nonNull(internalSchema.get$ref())) {
        final String[] pathObjectRef = internalSchema.get$ref().split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        fieldObjectArrayList.addAll(processFieldObjectList(schemaToProcess, fileSpec));
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

  private static List<SchemaFieldObject> processFieldObjectList(final Schema<?> schema, final FileSpec fileSpec) {
    final var mapperProperties = new HashMap<>(schema.getProperties());
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    mapperProperties.forEach((key, value) -> {
      final var enumValues = value.getEnum();
      if (CollectionUtils.isNotEmpty(enumValues)) {
        processEnumField(key, value, fileSpec, fieldObjectArrayList, enumValues, schema);
      } else {
        final var field = SchemaFieldObject.builder().baseName(key).dataTypeSimple(MapperUtil.getSimpleType(value, fileSpec)).build();
        setFieldType(field, value, schema, fileSpec, key);
        fieldObjectArrayList.add(field);
      }
    });
    return fieldObjectArrayList;
  }

  private static void setFieldType(final SchemaFieldObject field, final Schema<?> value, final Schema<?> schema, final FileSpec fileSpec, final String key) {
    field.setRequired(Objects.nonNull(schema.getRequired()) && schema.getRequired().contains(key));
    if (value instanceof ArraySchema) {
      final var typeArray = MapperUtil.getTypeArray((ArraySchema) value, fileSpec);
      field.setDataType(typeArray);
      field.setImportClass(getImportClass(typeArray));
    } else if (value instanceof MapSchema) {
      final var typeMap = MapperUtil.getTypeMap((MapSchema) value, fileSpec);
      field.setDataTypeSimple(MAP);
      field.setDataType(typeMap);
      field.setImportClass(getImportClass(typeMap));
    } else if (Objects.nonNull(value.getType()) && value.getType().equals("object")) {
      var typeObject = "";
      if (StringUtils.isNotBlank(value.get$ref())) {
        final String[] pathObjectRef = schema.get$ref().split("/");
        typeObject = MapperUtil.getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
      }
      field.setImportClass(getImportClass(typeObject));
      field.setDataType(typeObject);
    }
  }

  private static void processEnumField(
      final String key, final Schema<?> value, final FileSpec fileSpec,
      final ArrayList<SchemaFieldObject> fieldObjectArrayList, final List<?> enumValues, final Schema<?> schema) {
    final var field = SchemaFieldObject.builder().baseName(key).dataTypeSimple("enum").build();
    field.setRequired(Objects.nonNull(schema.getRequired()) && schema.getRequired().contains(key));
    final var dataType = MapperUtil.getSimpleType(value, fileSpec);
    field.setDataType(dataType);

    final HashMap<String, String> enumValuesMap = new HashMap<>();

    for (var enumValue : enumValues) {
      String valueName = enumValue.toString();
      valueName = valueName.replaceAll("\\.", "_DOT_");

      switch (dataType) {
        case INTEGER:
          enumValuesMap.put("INTEGER_" + valueName, enumValue.toString());
          break;
        case LONG:
          enumValuesMap.put("LONG_" + valueName, enumValue.toString() + "l");
          break;
        case DOUBLE:
          enumValuesMap.put("DOUBLE_" + valueName, enumValue.toString());
          break;
        case FLOAT:
          enumValuesMap.put("FLOAT_" + valueName, enumValue.toString() + "f");
          break;
        case BIG_DECIMAL:
          enumValuesMap.put("BIG_DECIMAL_" + valueName, "new BigDecimal(\"" + enumValue.toString() + "\")");
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
    return StringUtils.isNotBlank(type) && !"String".equals(type) && !"Integer".equals(type) ? type : "";
  }

}
