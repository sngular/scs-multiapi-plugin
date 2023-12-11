/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.util;

import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.TimeType;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperUtil {

  public static final String INTEGER = "integer";

  public static final String DOUBLE = "double";

  public static final String FLOAT = "float";

  public static final String NUMBER = "number";

  public static final String INT_64 = "int64";

  public static final String LONG = "long";

  public static final String DATE = "date";

  public static final String DATE_TIME = "date-time";

  public static final String BIG_DECIMAL = "BigDecimal";

  public static final String LOCAL_DATE = "LocalDate";

  public static final String LOCAL_DATE_TIME = "LocalDateTime";

  public static final String ZONED_DATE_TIME = "ZonedDateTime";

  public static final String REF = "$ref";

  private static final String DIVISOR = "([./])";

  private static final String SLASH = "/";

  private MapperUtil() {}

  public static String getSimpleType(final JsonNode schema, final String prefix, final String suffix,
      final TimeType useTimeType) {
    String type = schema.textValue();
    if (schema.has("type")) {
      type = schema.get("type").textValue();
      String format = null;
      if ("string".equalsIgnoreCase(type)) {
        type = "String";
      }
      if (schema.has("format")) {
        format = schema.get("format").textValue();
      }
      if ("string".equalsIgnoreCase(type)) {
        type = formatTypeOfString(format, useTimeType);
      } else if (NUMBER.equalsIgnoreCase(type)) {
        if (FLOAT.equalsIgnoreCase(format)) {
          type = FLOAT;
        } else if (DOUBLE.equalsIgnoreCase(format)) {
          type = DOUBLE;
        } else {
          type = BIG_DECIMAL;
        }
      } else if (INTEGER.equalsIgnoreCase(type)) {
        if (INT_64.equalsIgnoreCase(format)) {
          type = LONG;
        } else {
          type = INTEGER;
        }
      }
    } else if (schema.has(REF)) {
      type = getRef(schema, prefix, suffix);
    }
    return type;
  }

  public static String formatTypeOfString(final String format, final TimeType useTimeType) {
    String type = "String";
    if (format != null) {
      if (DATE_TIME.equalsIgnoreCase(format)) {
        switch(useTimeType) {
        case ZONED:
            type = ZONED_DATE_TIME;
            break;
          default:
            type = LOCAL_DATE_TIME;
        }

      } else if (DATE.equalsIgnoreCase(format)) {
        type = LOCAL_DATE;
      }
    }
    return type;
  }

  public static String getRef(final JsonNode schema, final String prefix, final String suffix) {
    return getPojoName(getRefClass(schema), prefix, suffix);
  }

  public static String getLongRefClass(final JsonNode schema) {
    final String[] pathObjectRef = getStrings(schema);
    return pathObjectRef[pathObjectRef.length - 2] + "/" + pathObjectRef[pathObjectRef.length - 1];
  }

  private static String[] getStrings(final JsonNode schema) {
    return splitName(schema.get(REF).textValue());
  }

  public static String getRefClass(final JsonNode schema) {
    final String[] pathObjectRef = getStrings(schema);
    return pathObjectRef[pathObjectRef.length - 1];
  }

  public static String getTypeMap(final JsonNode mapSchema, final String prefix, final String suffix, final TimeType useTimeType) {
    var typeMap = "";
    final var mapNode = mapSchema.get("additionalProperties");
    final var mapValueType = mapNode.findPath("type");
    typeMap = getCollectionType(mapNode, mapValueType, prefix, suffix, useTimeType);
    return typeMap;
  }

  public static String getTypeArray(final JsonNode array, final String prefix, final String suffix, final TimeType useTimeType) {
    var typeArray = "";
    final var arrayNode = array.get("items");
    final JsonNode mapValueType;
    if (arrayNode.has("type")) {
      mapValueType = arrayNode.get("type");
    } else {
      mapValueType = arrayNode.get(REF);
    }
    typeArray = getCollectionType(arrayNode, mapValueType, prefix, suffix, useTimeType);
    return typeArray;
  }

  private static String getCollectionType(final JsonNode mapNode, final JsonNode mapValueType, final String prefix,
      final String suffix, final TimeType useTimeType) {
    var typeMap = mapValueType.textValue();
    if (!typeMap.contains("#")) {
      typeMap = getSimpleType(mapNode, prefix, suffix, useTimeType);
    } else {
      final var valueSchema = mapNode.findPath(REF);
      if (Objects.nonNull(valueSchema)) {
        getRef(valueSchema, prefix, suffix);
      }
    }
    return typeMap;
  }

  public static String getPojoName(final String namePojo, final String prefix, final String suffix) {
    return StringUtils.defaultIfBlank(prefix, "")
           + StringUtils.capitalize(namePojo)
           + StringUtils.defaultIfBlank(suffix, "");
  }

  public static String[] splitName(final String name) {
    return ArrayUtils.removeAllOccurrences(name.split(DIVISOR), "");
  }

  public static String buildKey(final String[] pathList) {
    final var arrayLength = pathList.length;
    return (arrayLength > 2 ? pathList[arrayLength - 2] + SLASH + pathList[arrayLength - 1] : pathList[0]).toUpperCase();
  }

  protected static Object getConstValue(final JsonNode schema) {
    return ApiTool.hasNode(schema, "const") ? ApiTool.getNodeAsObject(schema, "const") : null;
  }
}
