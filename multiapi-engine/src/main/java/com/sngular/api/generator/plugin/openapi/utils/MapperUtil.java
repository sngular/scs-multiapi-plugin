/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.util.Objects;

import com.sngular.api.generator.plugin.openapi.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperUtil {

  private MapperUtil() {}

  public static String getSimpleType(final Schema<?> schema, final SpecFile specFile) {
    final String type;
    if (TypeConstants.NUMBER.equalsIgnoreCase(schema.getType())) {
      if (TypeConstants.FLOAT.equalsIgnoreCase(schema.getFormat())) {
        type = TypeConstants.FLOAT;
      } else if (TypeConstants.DOUBLE.equalsIgnoreCase(schema.getFormat())) {
        type = TypeConstants.DOUBLE;
      } else {
        type = TypeConstants.BIG_DECIMAL;
      }
    } else if (TypeConstants.INTEGER.equalsIgnoreCase(schema.getType())) {
      if (TypeConstants.INT_64.equalsIgnoreCase(schema.getFormat())) {
        type = TypeConstants.LONG;
      } else {
        type = TypeConstants.INTEGER;
      }
    } else if (Objects.nonNull(schema.get$ref())) {
      final String[] pathObjectRef = schema.get$ref().split("/");
      type = getPojoName(pathObjectRef[pathObjectRef.length - 1], specFile);
    } else if (TypeConstants.INT_32.equalsIgnoreCase(schema.getType()) || TypeConstants.INT_64.equalsIgnoreCase(schema.getType())) {
      type = TypeConstants.INTEGER;
    } else if (schema instanceof ArraySchema) {
      type = TypeConstants.ARRAY;
    } else {
      type = ObjectUtils.defaultIfNull(schema.getType(), TypeConstants.OBJECT);
    }
    return type;
  }

  public static String getSimpleType(final Object schema) {
    final String type;
    if (schema instanceof Boolean) {
      type = TypeConstants.OBJECT;
    } else if (schema instanceof Integer) {
      type = TypeConstants.INTEGER;
    } else if (schema instanceof Float) {
      type = TypeConstants.FLOAT;
    } else if (schema instanceof Double) {
      type = TypeConstants.DOUBLE;
    } else {
      type = TypeConstants.STRING;
    }
    return type;
  }

  public static String getTypeMap(final MapSchema mapSchema, final SpecFile specFile) {
    var typeMap = "";
    if (mapSchema.getAdditionalProperties() instanceof StringSchema) {
      typeMap = TypeConstants.STRING;
    } else if (mapSchema.getAdditionalProperties() instanceof IntegerSchema) {
      typeMap = TypeConstants.INTEGER;
    } else {
      final Schema<?> schema = (Schema<?>) mapSchema.getAdditionalProperties();
      if (StringUtils.isNotBlank(schema.get$ref())) {
        final String[] pathObjectRef = schema.get$ref().split("/");
        typeMap = getPojoName(pathObjectRef[pathObjectRef.length - 1], specFile);
      }
    }
    return typeMap;
  }

  public static String getTypeArray(final ArraySchema array, final SpecFile specFile) {
    var typeArray = "";
    if (array.getItems() instanceof StringSchema) {
      typeArray = TypeConstants.STRING;
    } else if (array.getItems() instanceof IntegerSchema) {
      typeArray = TypeConstants.INTEGER;
    } else if (StringUtils.isNotBlank(array.getItems().get$ref())) {
      final String[] pathObjectRef = array.getItems().get$ref().split("/");
      typeArray = getPojoName(pathObjectRef[pathObjectRef.length - 1], specFile);
    }
    return typeArray;
  }

  public static String getPojoName(final String namePojo, final SpecFile specFile) {
    return (StringUtils.isNotBlank(specFile.getModelNamePrefix()) ? specFile.getModelNamePrefix() : "")
           + StringUtils.capitalize(namePojo)
           + (StringUtils.isNotBlank(specFile.getModelNameSuffix()) ? specFile.getModelNameSuffix() : "");
  }

}
