/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.util.Objects;

import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.BIG_DECIMAL;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.OBJECT;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.INTEGER;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.LONG;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.FLOAT;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.DOUBLE;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.STRING;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.ARRAY;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.NUMBER;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.INT_32;
import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.INT_64;

public class MapperUtil {

  private MapperUtil() {}

  public static String getSimpleType(final Schema<?> schema, final SpecFile specFile) {
    final String type;
    if (NUMBER.equalsIgnoreCase(schema.getType())) {
      if (FLOAT.equalsIgnoreCase(schema.getFormat())) {
        type = FLOAT;
      } else if (DOUBLE.equalsIgnoreCase(schema.getFormat())) {
        type = DOUBLE;
      } else {
        type = BIG_DECIMAL;
      }
    } else if (INTEGER.equalsIgnoreCase(schema.getType())) {
      if (INT_64.equalsIgnoreCase(schema.getFormat())) {
        type = LONG;
      } else {
        type = INTEGER;
      }
    } else if (Objects.nonNull(schema.get$ref())) {
      final String[] pathObjectRef = schema.get$ref().split("/");
      type = getPojoName(pathObjectRef[pathObjectRef.length - 1], specFile);
    } else if (INT_32.equalsIgnoreCase(schema.getType()) || INT_64.equalsIgnoreCase(schema.getType())) {
      type = INTEGER;
    } else if (schema instanceof ArraySchema) {
      type = ARRAY;
    } else {
      type = ObjectUtils.defaultIfNull(schema.getType(), OBJECT);
    }
    return type;
  }

  public static String getSimpleType(final Object schema) {
    final String type;
    if (schema instanceof Boolean) {
      type = OBJECT;
    } else if (schema instanceof Integer) {
      type = INTEGER;
    } else if (schema instanceof Float) {
      type = FLOAT;
    } else if (schema instanceof Double) {
      type = DOUBLE;
    } else {
      type = STRING;
    }
    return type;
  }

  public static String getTypeMap(final MapSchema mapSchema, final SpecFile specFile) {
    var typeMap = "";
    if (mapSchema.getAdditionalProperties() instanceof StringSchema) {
      typeMap = STRING;
    } else if (mapSchema.getAdditionalProperties() instanceof IntegerSchema) {
      typeMap = INTEGER;
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
      typeArray = STRING;
    } else if (array.getItems() instanceof IntegerSchema) {
      typeArray = INTEGER;
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
