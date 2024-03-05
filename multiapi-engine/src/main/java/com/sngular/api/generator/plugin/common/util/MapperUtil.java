/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.util;

import java.nio.file.Path;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.IOperationObject;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.common.model.TimeType;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.SchemaUtil;
import com.sngular.api.generator.plugin.openapi.model.GlobalObject;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperUtil {

  private static final String DIVISOR = "([./])";

  public static final String REF = "$ref";

  private MapperUtil() {}

  public static String getSimpleType(final JsonNode schema, final IOperationObject operationObject, final TimeType useTimeType) {
    String type = ApiTool.getType(schema);
    if (ApiTool.hasType(schema)) {
      type = ApiTool.getType(schema);
      String format = null;
      if (ApiTool.hasFormat(schema)) {
        format = ApiTool.getFormat(schema);
      }
      if ("string".equalsIgnoreCase(type)) {
        type = formatTypeOfString(format, useTimeType);
      } else if (checkIfNumber(type)) {
        type = processNumber(schema);
      } else if (ApiTool.isObject(schema)) {
        type = ObjectUtils.defaultIfNull(type, TypeConstants.OBJECT);
      }
    } else if (ApiTool.hasRef(schema)) {
      type = getPojoName(getRefSchemaName(schema), operationObject);
    } else if (TypeConstants.ARRAY.equalsIgnoreCase(type)) {
      type = TypeConstants.ARRAY;
    }
    return type;
  }

  private static SchemaFieldObjectType getSchemaType(
      final JsonNode schema, final String pojoName, final IOperationObject specFile, final GlobalObject globalObject,
      final Path filePath, final Path baseDir) {
    SchemaFieldObjectType type = null;

    if (ApiTool.hasRef(schema)) {
      final String refSchemaPojoName = MapperUtil.getRef(schema, specFile);
      final JsonNode refSchema = SchemaUtil.solveRef(ApiTool.getRefValue(schema), globalObject.getSchemaMap(),
                                                     baseDir.resolve(filePath).getParent());
      type = getSchemaType(refSchema, refSchemaPojoName, specFile, globalObject, filePath, baseDir);
    } else if (ApiTool.hasAdditionalProperties(schema)) {
      type = getMapSchemaType(schema, pojoName, specFile, globalObject, filePath, baseDir);
    } else if (ApiTool.isDateTime(schema)) {
      type = new SchemaFieldObjectType(MapperUtil.getDateType(schema, specFile));
    } else if (ApiTool.hasType(schema)) {
      type = getObjectOrType(schema, pojoName, specFile, globalObject, filePath, baseDir);
    } else if (ApiTool.isComposed(schema)) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
    }

    return type;
  }

  private static SchemaFieldObjectType getObjectOrType(
      final JsonNode schema, final String pojoName, final IOperationObject specFile, final GlobalObject globalObject,
      final Path filePath, final Path baseDir) {
    return switch (ApiTool.getType(schema)) {
      case TypeConstants.OBJECT -> SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
      case TypeConstants.INTEGER -> new SchemaFieldObjectType(getIntegerFormat(schema));
      case TypeConstants.NUMBER -> new SchemaFieldObjectType(getNumberFormat(schema));
      case TypeConstants.BOOLEAN -> new SchemaFieldObjectType(TypeConstants.BOOLEAN);
      case TypeConstants.ARRAY -> new SchemaFieldObjectType(TypeConstants.ARRAY, getSchemaType(ApiTool.getItems(schema), pojoName, specFile, globalObject, filePath, baseDir));
      default -> new SchemaFieldObjectType(TypeConstants.STRING);
    };
  }

  private static String getIntegerFormat(final JsonNode schema) {
    return TypeConstants.INT_64.equalsIgnoreCase(ApiTool.getFormat(schema)) ? TypeConstants.LONG : TypeConstants.INTEGER;
  }

  private static String getNumberFormat(final JsonNode schema) {
    final String typeName;
    if (TypeConstants.FLOAT.equalsIgnoreCase(ApiTool.getFormat(schema))) {
      typeName = TypeConstants.FLOAT;
    } else if (TypeConstants.DOUBLE.equalsIgnoreCase(ApiTool.getFormat(schema))) {
      typeName = TypeConstants.DOUBLE;
    } else {
      typeName = TypeConstants.INTEGER;
    }

    return typeName;
  }


  private static SchemaFieldObjectType getMapSchemaType(
      final JsonNode schema, final String pojoName, final IOperationObject specFile, final GlobalObject globalObject,
      final Path filePath, final Path baseDir) {
    final SchemaFieldObjectType type;

    final JsonNode addPropObj = ApiTool.getAdditionalProperties(schema);
    if (ApiTool.hasProperties(schema)) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.OBJECT, pojoName);
    } else if (TypeConstants.BOOLEAN.equalsIgnoreCase(ApiTool.getType(addPropObj))) {
      type = SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, TypeConstants.OBJECT);
    } else {
      type = new SchemaFieldObjectType(TypeConstants.MAP, getSchemaType(addPropObj, pojoName, specFile, globalObject, filePath, baseDir));
    }

    return type;
  }

  public static String formatTypeOfString(final String format, final TimeType useTimeType) {
    String type = "String";
    if (StringUtils.isEmpty(format)) {
      if (TypeConstants.DATE_TIME.equalsIgnoreCase(format)) {
        if (Objects.requireNonNull(useTimeType) == TimeType.ZONED) {
          type = TypeConstants.ZONED_DATE_TIME;
        } else {
          type = TypeConstants.LOCAL_DATE_TIME;
        }
      } else if (TypeConstants.DATE.equalsIgnoreCase(format)) {
        type = TypeConstants.LOCAL_DATE;
      }
    }
    return type;
  }
  
  private static boolean checkIfNumber(final String nodeType) {
    return TypeConstants.NUMBER.equalsIgnoreCase(nodeType)
           || TypeConstants.INTEGER.equalsIgnoreCase(nodeType)
           || TypeConstants.INT_32.equalsIgnoreCase(nodeType)
           || TypeConstants.INT_64.equalsIgnoreCase(nodeType);
  }

  private static String processNumber(final JsonNode schema) {

    final var nodeType = ApiTool.getType(schema);
    final var formatType = ApiTool.getFormat(schema);
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

  public static String getPojoName(final String namePojo, final IOperationObject specFile) {
    return StringUtils.defaultIfBlank(specFile.getModelNamePostfix(), "")
           + StringUtils.capitalize(namePojo)
           + StringUtils.defaultIfBlank(specFile.getModelNameSuffix(), "");
  }

  public static String getRefSchemaName(final JsonNode parameter) {
    final String[] pathObjectRef = ApiTool.getRefValue(parameter).split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  public static String getRefSchemaName(final String parameter) {
    final String[] pathObjectRef = parameter.split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  public static String getTypeArray(final JsonNode array, final IOperationObject specFile, final TimeType useTimeType) {
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

  public static String getTypeMap(final JsonNode mapSchema, final IOperationObject specFile, final TimeType useTimeType) {
    var typeMap = "";
    final var mapNode = mapSchema.get("additionalProperties");
    final var mapValueType = mapNode.findPath("type");
    typeMap = getCollectionType(mapNode, mapValueType, specFile, useTimeType);
    return typeMap;
  }

  private static String getCollectionType(
      final JsonNode mapNode, final JsonNode mapValueType, final IOperationObject specFile, final TimeType useTimeType) {
    var typeMap = mapValueType.textValue();
    if (!typeMap.contains("#")) {
      typeMap = getSimpleType(mapNode, specFile, useTimeType);
    } else {
      final var valueSchema = ApiTool.getRef(mapNode);
      if (Objects.nonNull(valueSchema)) {
        getRef(valueSchema, specFile);
      }
    }
    return typeMap;
  }

  public static String getRef(final JsonNode schema, final IOperationObject specFile) {
    return getPojoName(getRefSchemaName(schema), specFile);
  }

  public static String getRefClass(final JsonNode schema) {
    final String[] pathObjectRef = getStrings(schema);
    return pathObjectRef[pathObjectRef.length - 1];
  }

  public static String getLongRefClass(final JsonNode schema) {
    final String[] pathObjectRef = getStrings(schema);
    return pathObjectRef[pathObjectRef.length - 2] + "/" + pathObjectRef[pathObjectRef.length - 1];
  }

  private static String[] getStrings(final JsonNode schema) {
    return splitName(schema.get(REF).textValue());
  }

  public static String[] splitName(final String name) {
    return ArrayUtils.removeAllOccurrences(name.split(DIVISOR), "");
  }
  
  public static String getDateType(final JsonNode schema, final IOperationObject specFile) {
    return switch (ApiTool.getFormat(schema)) {
      case "date" -> switch (specFile.getUseTimeType()) {
        case ZONED -> TypeConstants.ZONED_DATE;
        case LOCAL -> TypeConstants.LOCAL_DATE;
        default -> TypeConstants.OFFSET_DATE;
      };
      case "date-time" -> switch (specFile.getUseTimeType()) {
        case ZONED -> TypeConstants.ZONED_DATE_TIME;
        case LOCAL -> TypeConstants.LOCAL_DATE_TIME;
        default -> TypeConstants.OFFSET_DATE_TIME;
      };
      default -> TypeConstants.LOCAL_DATE_TIME;
    };
  }

  protected static Object getConstValue(final JsonNode schema) {
    return ApiTool.hasNode(schema, "const") ? ApiTool.getNodeAsObject(schema, "const") : null;
  }
}
