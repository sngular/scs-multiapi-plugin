/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.openapi.model.TypeConstants;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperUtil {

  private MapperUtil() {}

  public static String getSimpleType(final JsonNode schema, final SpecFile specFile) {
    final String type;
    final var nodeType = ApiTool.getType(schema);
    if (checkIfNumber(nodeType)) {
      type = processNumber(schema);
    } else if (ApiTool.hasRef(schema)) {
      type = getPojoName(getRefSchemaName(schema), specFile);
    } else if (TypeConstants.ARRAY.equalsIgnoreCase(nodeType)) {
      type = TypeConstants.ARRAY;
    } else {
      type = ObjectUtils.defaultIfNull(nodeType, TypeConstants.OBJECT);
    }
    return type;
  }

  public static String getRefSchemaName(final JsonNode parameter) {
    final String[] pathObjectRef = ApiTool.getRefValue(parameter).split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  private static boolean checkIfNumber(final String nodeType) {
    return TypeConstants.NUMBER.equalsIgnoreCase(nodeType) || TypeConstants.INTEGER.equalsIgnoreCase(nodeType)
           || TypeConstants.INT_32.equalsIgnoreCase(nodeType) || TypeConstants.INT_64.equalsIgnoreCase(nodeType);
  }

  private static String processNumber(final JsonNode schema) {

    final var nodeType = schema.get("type").asText();
    final var formatType = schema.has("format") ? schema.get("format").asText() : null;
    String type = TypeConstants.INTEGER;
    if (TypeConstants.NUMBER.equalsIgnoreCase(nodeType)) {
      if (TypeConstants.FLOAT.equalsIgnoreCase(formatType)) {
        type = TypeConstants.FLOAT;
      } else if (TypeConstants.DOUBLE.equalsIgnoreCase(formatType)) {
        type = TypeConstants.DOUBLE;
      } else {
        type = TypeConstants.BIG_DECIMAL;
      }
    } else if (TypeConstants.INTEGER.equalsIgnoreCase(nodeType)) {
      if (TypeConstants.INT_64.equalsIgnoreCase(formatType)) {
        type = TypeConstants.LONG;
      } else {
        type = TypeConstants.INTEGER;
      }
    } else if (TypeConstants.INT_32.equalsIgnoreCase(nodeType) || TypeConstants.INT_64.equalsIgnoreCase(nodeType)) {
      type = TypeConstants.INTEGER;
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

  public static String getTypeArray(final JsonNode array, final SpecFile specFile) {
    var typeArray = "";
    if (ApiTool.isString(ApiTool.getItems(array))) {
      typeArray = TypeConstants.STRING;
    } else if (ApiTool.isNumber(ApiTool.getItems(array))) {
      typeArray = ApiTool.getNumberType(ApiTool.getItems(array));
    } else if (ApiTool.hasRef(ApiTool.getItems(array))) {
      final String[] pathObjectRef = ApiTool.getRefValue(ApiTool.getItems(array)).split("/");
      typeArray = getPojoName(pathObjectRef[pathObjectRef.length - 1], specFile);
    }
    return typeArray;
  }

  public static String getPojoName(final String namePojo, final SpecFile specFile) {
    return (StringUtils.isNotBlank(specFile.getModelNamePrefix()) ? specFile.getModelNamePrefix() : "")
           + StringUtils.capitalize(namePojo)
           + (StringUtils.isNotBlank(specFile.getModelNameSuffix()) ? specFile.getModelNameSuffix() : "");
  }

  public static String getRefSchemaName(final String parameter) {
    final String[] pathObjectRef = parameter.split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }
}
