/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import net.coru.api.generator.plugin.openapi.model.SchemaFieldObject;
import net.coru.api.generator.plugin.openapi.model.SchemaObject;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {

  private static final String ARRAY = "array";

  private static final String MAP = "map";

  private MapperContentUtil() {}

  public static SchemaObject mapComponentToSchemaObject(final Schema<?> schema, final String nameSchema, final FileSpec fileSpec, final String modelPackage) {
    final var listSchema = getFields(schema, fileSpec);

    return SchemaObject.builder()
                       .schemaName(schema.getName())
                       .className(MapperUtil.getPojoName(nameSchema, fileSpec))
                       .importList(getImportList(listSchema, modelPackage))
                       .fieldObjectList(listSchema)
                       .build();
  }

  private static List<String> getImportList(final List<SchemaFieldObject> fieldObjectList, final String modelPackage) {
    final var listHashMap = new HashMap<String, List<String>>();
    final var importList = new ArrayList<String>();

    fieldObjectList.forEach(fieldObject -> {
      if (Objects.nonNull(fieldObject.getDataTypeSimple())) {
        if (fieldObject.getDataTypeSimple().equals(ARRAY)) {
          listHashMap.computeIfAbsent(ARRAY, key -> List.of("java.util.List", "java.util.ArrayList"));
        } else if (Objects.equals(fieldObject.getDataTypeSimple(), MAP)) {
          listHashMap.computeIfAbsent(MAP, key -> List.of("java.util.Map", "java.util.HashMap"));
        }
      }
      if (StringUtils.isNotBlank(fieldObject.getImportClass()) && !listHashMap.containsKey(fieldObject.getImportClass())) {
        listHashMap.put(fieldObject.getImportClass(), List.of(modelPackage + "." + fieldObject.getImportClass()));
      }
    });

    if (!listHashMap.isEmpty()) {
      listHashMap.forEach((key, value) -> importList.addAll(value));
    }

    return importList;
  }

  private static List<SchemaFieldObject> getFields(final Schema<?> schema, final FileSpec fileSpec) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    if (Objects.nonNull(schema.getProperties())) {
      final var mapperProperties = new HashMap<>(schema.getProperties());

      mapperProperties.forEach((key, value) -> {
        final var field = SchemaFieldObject.builder().baseName(key).dataTypeSimple(MapperUtil.getSimpleType(value, fileSpec)).build();
        setFieldType(field, value, schema, fileSpec);
        fieldObjectArrayList.add(field);
      });
    }
    return fieldObjectArrayList;
  }

  private static void setFieldType(final SchemaFieldObject field, final Schema<?> value, final Schema<?> schema, final FileSpec fileSpec) {
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

  private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !"String".equals(type) && !"Integer".equals(type) ? type : "";
  }

}
