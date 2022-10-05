/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.util;

import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
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

  public static String getSimpleType(final JsonNode schema, final String prefix, final String suffix) {
    final String type;
    String nodeType = null;
    if (schema.has("type")) {
      nodeType = schema.get("type").textValue();
    }
    String format = null;
    if (schema.has("format")) {
      format = schema.get("format").textValue();
    }
    if (NUMBER.equalsIgnoreCase(nodeType)) {
      if (FLOAT.equalsIgnoreCase(format)) {
        type = FLOAT;
      } else if (DOUBLE.equalsIgnoreCase(format)) {
        type = DOUBLE;
      } else {
        type = BIG_DECIMAL;
      }
    } else if (INTEGER.equalsIgnoreCase(nodeType)) {
      if (INT_64.equalsIgnoreCase(format)) {
        type = LONG;
      } else {
        type = INTEGER;
      }
    } else if (Objects.nonNull(schema.findPath("$ref"))) {
      final String[] pathObjectRef = schema.findValue("$ref").textValue().split("/");
      type = getPojoName(pathObjectRef[pathObjectRef.length - 1], prefix, suffix);
    } else {
      type = nodeType;
    }
    return type;
  }

  public static String getTypeMap(final JsonNode mapSchema, final String prefix, final String suffix) {
    var typeMap = "";
    final var mapNode = mapSchema.get("additionalProperties");
    final var mapValueType = mapNode.findPath("type");
    typeMap = getCollectionType(mapNode, mapValueType, prefix, suffix);
    return typeMap;
  }

  public static String getTypeArray(final JsonNode array, final String prefix, final String suffix) {
    var typeArray = "";
    final var arrayNode = array.get("items");
    final var mapValueType = arrayNode.findPath("type");
    typeArray = getCollectionType(arrayNode, mapValueType, prefix, suffix);
    return typeArray;
  }

  private static String getCollectionType(final JsonNode mapNode, final JsonNode mapValueType, final String prefix, final String suffix) {
    var typeMap = "";
    if (Objects.nonNull(mapValueType)) {
      if ("string".equalsIgnoreCase(mapValueType.textValue())) {
        typeMap = "String";
      } else if ("integer".equalsIgnoreCase(mapValueType.textValue())) {
        typeMap = "Integer";
      } else {
        typeMap = mapValueType.textValue();
      }
    } else {
      final var valueSchema = mapNode.findPath("$ref");
      if (Objects.nonNull(valueSchema)) {
        final String[] pathObjectRef = valueSchema.textValue().split("/");
        typeMap = getPojoName(pathObjectRef[pathObjectRef.length - 1], prefix, suffix);
      }
    }
    return typeMap;
  }

  public static String getPojoName(final String namePojo, final String prefix, final String suffix) {
    return StringUtils.defaultIfBlank(prefix, "")
           + namePojo
           + StringUtils.defaultIfBlank(suffix, "");
  }

}
