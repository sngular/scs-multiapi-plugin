/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.openapi.utils;

import static com.corunet.api.generator.plugin.openapi.utils.MapperUtil.getPojoName;
import static com.corunet.api.generator.plugin.openapi.utils.MapperUtil.getSimpleType;
import static com.corunet.api.generator.plugin.openapi.utils.MapperUtil.getTypeArray;
import static com.corunet.api.generator.plugin.openapi.utils.MapperUtil.getTypeMap;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import com.corunet.api.generator.plugin.openapi.model.SchemaFieldObject;
import com.corunet.api.generator.plugin.openapi.model.SchemaObject;
import com.corunet.api.generator.plugin.openapi.parameter.FileSpec;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;

public class MapperContentUtil {


  public static SchemaObject mapComponentToSchemaObject(
      Schema schema, String nameSchema,
      FileSpec fileSpec, String modelPackage) {
    var listSchema = getFields(schema, fileSpec);

    return SchemaObject.builder()
                       .description(schema.getDescription())
                       .schemaName(schema.getName())
                       .className(getPojoName(nameSchema, fileSpec))
                       .importList(getImportList(listSchema, modelPackage))
                       .fieldObjectList(listSchema)
                       .build();
  }

  private static List<String> getImportList(
      List<SchemaFieldObject> fieldObjectList,
      String modelPackage) {
    var listHashMap = new HashMap<String, List<String>>();
    var importList = new ArrayList<String>();

    fieldObjectList.forEach(fieldObject -> {
      if (fieldObject.getDataTypeSimple().equals("array") && !listHashMap.containsKey("array")) {
        var arrayImport = new ArrayList<String>();
        arrayImport.add("java.util.List");
        arrayImport.add("java.util.ArrayList");
        listHashMap.put("array", arrayImport);
      }

      if (Objects.equals(fieldObject.getDataTypeSimple(), "map") && !listHashMap.containsKey("map")) {
        var arrayImport = new ArrayList<String>();
        arrayImport.add("java.util.Map");
        arrayImport.add("java.util.HashMap");
        listHashMap.put("map", arrayImport);
      }

      if (StringUtils.isNotBlank(fieldObject.getImportClass())
          && !listHashMap.containsKey(fieldObject.getImportClass())) {
        var arrayImport = new ArrayList<String>();
        arrayImport.add(modelPackage + "." + fieldObject.getImportClass());
        listHashMap.put(fieldObject.getImportClass(), arrayImport);
      }

    });

    if (!listHashMap.isEmpty()) {
      listHashMap.forEach((key, value) -> importList.addAll(value));
    }

    return importList;
  }

  private static List<SchemaFieldObject> getFields(Schema schema, FileSpec fileSpec) {
    var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();

    var mapperProperties = new HashMap<String, Schema>(schema.getProperties());

    mapperProperties.forEach((key, value) -> {
      var field = SchemaFieldObject.builder()
                                   .baseName(key)
                                   .dataTypeSimple(getSimpleType(value))
                                   .build();
      if (value instanceof ArraySchema) {
        var typeArray = getTypeArray((ArraySchema) value, fileSpec);
        field.setDataType(typeArray);
        field.setImportClass(getImportClass(typeArray));
      } else if (value instanceof MapSchema) {
        var typeMap = getTypeMap((MapSchema) value, fileSpec);
        field.setDataTypeSimple("map");
        field.setDataType(typeMap);
        field.setImportClass(getImportClass(typeMap));
      } else if (value.getType().equals("object")) {
        var typeObject = "";
        if (StringUtils.isNotBlank(value.get$ref())) {
          String[] pathObjectRef = schema.get$ref().split("/");
          typeObject = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
        }
        field.setImportClass(getImportClass(typeObject));
        field.setDataType(typeObject);
      }

      fieldObjectArrayList.add(field);

    });
    return fieldObjectArrayList;
  }

  private static String getImportClass(String type) {
    return StringUtils.isNotBlank(type) && (!type.equals("String") && !type.equals("Integer")) ? type : "";
  }

}
