package com.corunet.api.generator.plugin.openapi.utils;

import com.corunet.api.generator.plugin.openapi.parameter.FileSpec;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.lang3.StringUtils;

public class MapperUtil {

  public static String getSimpleType(Schema schema) {
    String type = "";
    if ("number".equalsIgnoreCase(schema.getType())) {
      if ("float".equalsIgnoreCase(schema.getFormat())) {
        type = "float";
      } else if ("double".equalsIgnoreCase(schema.getFormat())) {
        type = "double";
      } else {
        type = "integer";
      }
    } else if ("integer".equalsIgnoreCase(schema.getType())) {
      if ("int64".equalsIgnoreCase(schema.getType())) {
        type = "long";
      } else {
        type = "integer";
      }
    } else {
      type = schema.getType();
    }
    return type;
  }

  public static String getTypeMap(MapSchema mapSchema, FileSpec fileSpec) {
    var typeMap = "";
    if (mapSchema.getAdditionalProperties() instanceof StringSchema) {
      typeMap = "String";
    } else if (mapSchema.getAdditionalProperties() instanceof IntegerSchema) {
      typeMap = "Integer";
    } else {
      Schema schema = (Schema) mapSchema.getAdditionalProperties();
      if (StringUtils.isNotBlank(schema.get$ref())) {
        String[] pathObjectRef = schema.get$ref().split("/");
        typeMap = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
      }
    }
    return typeMap;
  }

  public static String getTypeArray(ArraySchema array, FileSpec fileSpec) {
    var typeArray = "";
    if (array.getItems() instanceof StringSchema) {
      typeArray = "String";
    } else if (array.getItems() instanceof IntegerSchema) {
      typeArray = "Integer";
    } else if (StringUtils.isNotBlank(array.getItems().get$ref())) {
      String[] pathObjectRef = array.getItems().get$ref().split("/");
      typeArray = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
    }
    return typeArray;
  }

  public static String getPojoName(String namePojo, FileSpec fileSpec) {
    return (StringUtils.isNotBlank(fileSpec.getModelNamePrefix()) ? fileSpec.getModelNamePrefix() : "")
           + namePojo
           + (StringUtils.isNotBlank(fileSpec.getModelNameSuffix()) ? fileSpec.getModelNameSuffix() : "");
  }

}
