/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperUtil {

  private static final String REF = "$ref";

  private static final String DIVISOR = "([./])";

  private static final String SLASH = "/";

  private MapperUtil() {}

  public static String getSimpleType(final JsonNode schema, final CommonSpecFile specFile) {
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

  public static String[] splitName(final String name) {
    return ArrayUtils.removeAllOccurrences(name.split(DIVISOR), "");
  }

  public static String packageToFolder(final String packageName) {
    return StringUtils.replace(packageName, ".", SLASH);
  }

  public static String getRefSchemaName(final JsonNode parameter) {
    final String[] pathObjectRef = ApiTool.getRefValue(parameter).split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  public static String getRefSchemaKey(final JsonNode parameter) {
    final String[] pathObjectRef = ApiTool.getRefValue(parameter).split("/");
    return StringUtils.upperCase(pathObjectRef[pathObjectRef.length - 2] + "/" + StringCaseUtils.titleToSnakeCase(pathObjectRef[pathObjectRef.length - 1]));
  }

  public static String getRefSchemaKey(final String parameter) {
    final String[] pathObjectRef = parameter.split("/");
    return StringUtils.upperCase(pathObjectRef[pathObjectRef.length - 2] + "/" + getSchemaKey(pathObjectRef[pathObjectRef.length - 1]));
  }

  public static String getKeySchemaName(final String parameter) {
    return StringCaseUtils.toCamelCase(getKey(parameter));
  }

  public static String getKey(final String keyString) {
    final String[] pathObjectRef = keyString.split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  public static String getSchemaKey(final String schemaName) {
    return StringCaseUtils.titleToSnakeCase(schemaName);
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
    }
    return type;
  }

  public static String getTypeArray(final JsonNode array, final CommonSpecFile specFile) {
    var typeArray = "";
    if (ApiTool.isString(ApiTool.getItems(array))) {
      typeArray = TypeConstants.STRING;
    } else if (ApiTool.isNumber(ApiTool.getItems(array))) {
      typeArray = ApiTool.getNumberType(ApiTool.getItems(array));
    } else if (ApiTool.hasRef(ApiTool.getItems(array))) {
      typeArray = getPojoName(MapperUtil.getRefSchemaName(ApiTool.getItems(array)), specFile);
    }
    return typeArray;
  }

  public static String getPojoName(final String namePojo, final CommonSpecFile specFile) {
    return (StringUtils.isNotBlank(specFile.getModelNamePrefix()) ? specFile.getModelNamePrefix() : "")
           + StringUtils.capitalize(namePojo)
           + (StringUtils.isNotBlank(specFile.getModelNameSuffix()) ? specFile.getModelNameSuffix() : "");
  }

  public static String getRef(final JsonNode schema, final CommonSpecFile specFile) {
    final String typeObject;
    typeObject = getPojoName(getRefSchemaName(schema), specFile);
    return typeObject;
  }

  public static String getDateType(final JsonNode schema, final CommonSpecFile specFile) {
    final String dateType;
    switch (ApiTool.getFormat(schema)) {

      case "date":
        switch (specFile.getUseTimeType()) {
          case ZONED:
            dateType = TypeConstants.ZONEDDATE;
            break;
          case OFFSET:
            dateType = TypeConstants.OFFSETDATE;
            break;
          default:
            dateType = TypeConstants.LOCALDATE;
        }
        break;
      case "date-time":
        switch (specFile.getUseTimeType()) {
          case ZONED:
            dateType = TypeConstants.ZONEDDATETIME;
            break;
          case OFFSET:
            dateType = TypeConstants.OFFSETDATETIME;
            break;
          default:
            dateType = TypeConstants.LOCALDATETIME;
        }
        break;
      default:
        dateType = TypeConstants.LOCALDATETIME;
    }
    return dateType;
  }

  public static String getRefClass(final JsonNode schema) {
    final String[] pathObjectRef = getStrings(schema);
    return pathObjectRef[pathObjectRef.length - 1];
  }

  private static String[] getStrings(final JsonNode schema) {
    return splitName(schema.get(REF).textValue());
  }

  public static String getLongRefClass(final JsonNode schema) {
    final String[] pathObjectRef = getStrings(schema);
    return pathObjectRef[pathObjectRef.length - 2] + "/" + pathObjectRef[pathObjectRef.length - 1];
  }
}
