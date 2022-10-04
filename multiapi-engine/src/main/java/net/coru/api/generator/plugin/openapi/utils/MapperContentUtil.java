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
import net.coru.api.generator.plugin.openapi.parameter.SpecFile;
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
      final Map<String, Schema> totalSchemas, final Schema<?> schema, final String nameSchema, final SpecFile specFile,
      final String modelPackage) {
    final var listSchema = getFields(totalSchemas, schema, specFile);

    return SchemaObject.builder()
                       .schemaName(schema.getName())
                       .className(MapperUtil.getPojoName(nameSchema, specFile))
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
      } else if (Objects.nonNull(fieldObject.getDataTypeSimple()) && fieldObject.getDataTypeSimple().equals(BIG_DECIMAL) ||
                 Objects.nonNull(fieldObject.getDataType()) && fieldObject.getDataType().equals(BIG_DECIMAL)) {
        listHashMap.computeIfAbsent(BIG_DECIMAL, key -> List.of("java.math.BigDecimal"));
      }
    }
  }

  private static List<SchemaFieldObject> getFields(final Map<String, Schema> totalSchemas, final Schema<?> schema, final SpecFile specFile) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    schemaCombinatorType = null;
    if (Objects.nonNull(schema.getProperties())) {
      fieldObjectArrayList.addAll(processFieldObjectList(schema, specFile));
    } else if (ARRAY.equalsIgnoreCase(schema.getType())) {
      final ArraySchema arraySchema = (ArraySchema) schema;
      fieldObjectArrayList.addAll(processFieldObjectList(arraySchema.getItems(), specFile));
    } else if (Objects.nonNull(schema.getAllOf())) {
      fieldObjectArrayList.addAll(processAllOf(totalSchemas, schema.getAllOf(), specFile));
      schemaCombinatorType = "allOf";
    } else if (Objects.nonNull(schema.getAnyOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getAnyOf(), specFile));
      schemaCombinatorType = "anyOf";
    } else if (Objects.nonNull(schema.getOneOf())) {
      fieldObjectArrayList.addAll(processAnyOfOneOf(totalSchemas, schema.getOneOf(), specFile));
      schemaCombinatorType = "oneOf";
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAllOf(final Map<String, Schema> totalSchemas, final List<Schema> schemaList, final SpecFile specFile) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (Schema<?> ref : schemaList) {
      if (Objects.nonNull(ref.get$ref())) {
        final String[] pathObjectRef = ref.get$ref().split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        fieldObjectArrayList.addAll(processFieldObjectList(schemaToProcess, specFile));
        for (var fieldObject : fieldObjectArrayList) {
          fieldObject.setRequired(true);
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAnyOfOneOf(final Map<String, Schema> totalSchemas, final List<Schema> schemaList, final SpecFile specFile) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    for (Schema<?> internalSchema : schemaList) {
      if (Objects.nonNull(internalSchema.get$ref())) {
        final String[] pathObjectRef = internalSchema.get$ref().split("/");
        final String schemaName = pathObjectRef[pathObjectRef.length - 1];
        final var schemaToProcess = totalSchemas.get(schemaName);
        fieldObjectArrayList.addAll(processFieldObjectList(schemaToProcess, specFile));
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

  private static List<SchemaFieldObject> processFieldObjectList(final Schema<?> schema, final SpecFile specFile) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    if (ARRAY.equalsIgnoreCase(schema.getType())) {
      final ArraySchema arraySchema = (ArraySchema) schema;
      if (Objects.nonNull(arraySchema.getItems()) && Objects.nonNull(arraySchema.getItems().get$ref())) {
        final String refSchemaName = getRef(arraySchema.getItems(), specFile);
        final var field = SchemaFieldObject.builder().baseName(refSchemaName).dataTypeSimple(MapperUtil.getSimpleType(arraySchema.getItems(), specFile)).build();
        setFieldType(field, arraySchema.getItems(), arraySchema.getItems(), specFile, refSchemaName);
        fieldObjectArrayList.add(field);
      }
    } else if (Objects.nonNull(schema.get$ref())) {
      final String refSchemaName = getRef(schema, specFile);
      final var field = SchemaFieldObject.builder().baseName(refSchemaName).dataTypeSimple(MapperUtil.getSimpleType(schema, specFile)).build();
      setFieldType(field, schema, schema, specFile, refSchemaName);
      fieldObjectArrayList.add(field);
    } else {
      final var mapperProperties = new HashMap<>(schema.getProperties());
      mapperProperties.forEach((key, value) -> {
        final var enumValues = value.getEnum();
        if (CollectionUtils.isNotEmpty(enumValues)) {
          processEnumField(key, value, specFile, fieldObjectArrayList, enumValues, schema);
        } else {
          final var field = SchemaFieldObject.builder().baseName(key).dataTypeSimple(MapperUtil.getSimpleType(value, specFile)).build();
          setFieldType(field, value, schema, specFile, key);
          fieldObjectArrayList.add(field);
        }
      });
    }
    return fieldObjectArrayList;
  }

  private static void setFieldType(final SchemaFieldObject field, final Schema<?> value, final Schema<?> schema, final SpecFile specFile, final String key) {
    field.setRequired(Objects.nonNull(schema.getRequired()) && schema.getRequired().contains(key));
    if (value instanceof ArraySchema) {
      final var typeArray = MapperUtil.getTypeArray((ArraySchema) value, specFile);
      field.setDataType(typeArray);
      field.setImportClass(getImportClass(typeArray));
    } else if (value instanceof MapSchema) {
      final var typeMap = MapperUtil.getTypeMap((MapSchema) value, specFile);
      field.setDataTypeSimple(MAP);
      field.setDataType(typeMap);
      field.setImportClass(getImportClass(typeMap));
    } else if (Objects.nonNull(value.getType()) && value.getType().equals("object")) {
      var typeObject = "";
      if (StringUtils.isNotBlank(value.get$ref())) {
        typeObject = getRef(schema, specFile);
      }
      field.setImportClass(getImportClass(typeObject));
      field.setDataType(typeObject);
    }
  }

  private static String getRef(final Schema<?> schema, final SpecFile specFile) {
    final String typeObject;
    final String[] pathObjectRef = schema.get$ref().split("/");
    typeObject = MapperUtil.getPojoName(pathObjectRef[pathObjectRef.length - 1], specFile);
    return typeObject;
  }

  private static void processEnumField(
      final String key, final Schema<?> value, final SpecFile specFile,
      final ArrayList<SchemaFieldObject> fieldObjectArrayList, final List<?> enumValues, final Schema<?> schema) {
    final var field = SchemaFieldObject.builder().baseName(key).dataTypeSimple("enum").build();
    field.setRequired(Objects.nonNull(schema.getRequired()) && schema.getRequired().contains(key));
    final var dataType = MapperUtil.getSimpleType(value, specFile);
    field.setDataType(dataType);

    final HashMap<String, String> enumValuesMap = new HashMap<>();

    for (var enumValue : enumValues) {
      String valueName = enumValue.toString();
      valueName = valueName.replace("\\.", "_DOT_");

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
