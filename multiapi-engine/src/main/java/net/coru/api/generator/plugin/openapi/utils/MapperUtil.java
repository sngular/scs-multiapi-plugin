/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import java.util.Objects;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import org.apache.commons.lang3.StringUtils;

public class MapperUtil {

  public static final String INTEGER = "integer";

  public static final String DOUBLE = "double";

  public static final String FLOAT = "float";

  public static final String NUMBER = "number";

  public static final String INT_64 = "int64";

  public static final String LONG = "long";

  public static final String BIG_DECIMAL = "bigDecimal";

  private MapperUtil() {}

  public static String getSimpleType(final Schema<?> schema, final FileSpec fileSpec) {
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
      type = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
    } else {
      type = schema.getType();
    }
    return type;
  }

  public static String getTypeMap(final MapSchema mapSchema, final FileSpec fileSpec) {
    var typeMap = "";
    if (mapSchema.getAdditionalProperties() instanceof StringSchema) {
      typeMap = "String";
    } else if (mapSchema.getAdditionalProperties() instanceof IntegerSchema) {
      typeMap = "Integer";
    } else {
      final Schema<?> schema = (Schema<?>) mapSchema.getAdditionalProperties();
      if (StringUtils.isNotBlank(schema.get$ref())) {
        final String[] pathObjectRef = schema.get$ref().split("/");
        typeMap = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
      }
    }
    return typeMap;
  }

  public static String getTypeArray(final ArraySchema array, final FileSpec fileSpec) {
    var typeArray = "";
    if (array.getItems() instanceof StringSchema) {
      typeArray = "String";
    } else if (array.getItems() instanceof IntegerSchema) {
      typeArray = "Integer";
    } else if (StringUtils.isNotBlank(array.getItems().get$ref())) {
      final String[] pathObjectRef = array.getItems().get$ref().split("/");
      typeArray = getPojoName(pathObjectRef[pathObjectRef.length - 1], fileSpec);
    }
    return typeArray;
  }

  public static String getPojoName(final String namePojo, final FileSpec fileSpec) {
    return (StringUtils.isNotBlank(fileSpec.getModelNamePrefix()) ? fileSpec.getModelNamePrefix() : "")
           + namePojo
           + (StringUtils.isNotBlank(fileSpec.getModelNameSuffix()) ? fileSpec.getModelNameSuffix() : "");
  }

}
