/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

public class MapperUtil {

  public static final String JAVA_TIME = "java.time.";

  private static final String REF = "$ref";

  private static final String[] DIVISOR = {"/", "-", "_"};

  private static final String SLASH = "/";

  private static final String PACKAGE_SEPARATOR_STR = ".";

  private MapperUtil() {
  }

  public static String getSimpleType(final JsonNode schema, final CommonSpecFile specFile) {
    final String type;
    final var nodeType = ApiTool.getType(schema);
    if (checkIfNumber(nodeType)) {
      type = processNumber(schema);
    } else if (ApiTool.hasRef(schema)) {
      type = getPojoName(getRefSchemaName(schema, null), specFile);
    } else if (TypeConstants.ARRAY.equalsIgnoreCase(nodeType)) {
      type = TypeConstants.ARRAY;
    } else {
      type = ObjectUtils.defaultIfNull(nodeType, TypeConstants.OBJECT);
    }
    return type;
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

  public static String getPojoName(final String namePojo, final CommonSpecFile specFile) {
    final String raw = namePojo == null ? "" : namePojo;
    final boolean hasSeparators = raw.indexOf('_') >= 0 || raw.indexOf('-') >= 0 || raw.indexOf(' ') >= 0;
    final String normalized = hasSeparators ? StringCaseUtils.toCamelCase(raw) : StringUtils.capitalize(raw);
    return (StringUtils.isNotBlank(specFile.getModelNamePrefix()) ? specFile.getModelNamePrefix() : "")
           + normalized
           + (StringUtils.isNotBlank(specFile.getModelNameSuffix()) ? specFile.getModelNameSuffix() : "");
  }

  public static String getRefSchemaName(final JsonNode parameter, String defaultSchemaName) {
    final String[] pathObjectRef = ApiTool.getRefValue(parameter).split("/");
    final String schemaName;
    if (pathObjectRef[pathObjectRef.length - 1].contains(".yml")
        || pathObjectRef[pathObjectRef.length - 1].contains(".yaml")
        || pathObjectRef[pathObjectRef.length - 1].contains(".json")) {
      schemaName = StringUtils.defaultIfEmpty(defaultSchemaName, "");
    } else {
      schemaName = pathObjectRef[pathObjectRef.length - 1];
    }
    return schemaName;
  }

  public static String[] splitReference(final String name) {
    if (0 < StringUtils.indexOf(name, "#")) {
      return StringUtils.split(name, "#");
    } else if (0 == StringUtils.indexOf(name, "#")) {
      return StringUtils.split(name, "#");
    } else {
      return ArrayUtils.addAll(new String[]{}, name, "");
    }
  }

  public static String packageToFolder(final String packageName) {
    return StringUtils.replace(packageName, ".", SLASH);
  }

  public static String getRefSchemaKey(final JsonNode parameter) {
    final String[] pathObjectRef = ApiTool.getRefValue(parameter).split("/");
    return StringUtils.upperCase(pathObjectRef[pathObjectRef.length - 2] + "/" + StringCaseUtils.titleToSnakeCase(pathObjectRef[pathObjectRef.length - 1]));
  }

  public static String getRefSchemaKey(final String parameter) {
    final String[] pathObjectRef = parameter.split("/");
    return StringUtils.upperCase(pathObjectRef[pathObjectRef.length - 2] + "/" + getSchemaKey(pathObjectRef[pathObjectRef.length - 1]));
  }

  public static String getSchemaKey(final String schemaName) {
    return StringCaseUtils.titleToSnakeCase(schemaName);
  }

  public static String getKeySchemaName(final String parameter) {
    return StringCaseUtils.toCamelCase(getKey(parameter));
  }

  public static String getKey(final String keyString) {
    final String[] pathObjectRef = keyString.split("/");
    return pathObjectRef[pathObjectRef.length - 1];
  }

  public static String getTypeArray(final JsonNode array, final CommonSpecFile specFile) {
    var typeArray = "";
    if (ApiTool.isString(ApiTool.getItems(array))) {
      typeArray = TypeConstants.STRING;
    } else if (ApiTool.isNumber(ApiTool.getItems(array))) {
      typeArray = ApiTool.getNumberType(ApiTool.getItems(array));
    } else if (ApiTool.hasRef(ApiTool.getItems(array))) {
      typeArray = getPojoName(MapperUtil.getRefSchemaName(ApiTool.getItems(array), null), specFile);
    }
    return typeArray;
  }

  public static String calculatePrefixName(final String namePojo, final CommonSpecFile specFile) {
    final String raw = namePojo == null ? "" : namePojo;
    final boolean hasSeparators = raw.indexOf('_') >= 0 || raw.indexOf('-') >= 0 || raw.indexOf(' ') >= 0;
    final String normalized = hasSeparators ? StringCaseUtils.toCamelCase(raw) : StringUtils.capitalize(raw);
    return (StringUtils.isNotBlank(specFile.getModelNamePrefix()) ? specFile.getModelNamePrefix() : "")
           + normalized;
  }

  public static String getPojoNameFromRef(final JsonNode schema, final CommonSpecFile specFile, String defaultPojoName) {
    String pojoName = getRefSchemaName(schema, defaultPojoName);
    if (!StringUtils.equalsIgnoreCase(pojoName, defaultPojoName)) {
      pojoName = getPojoName(pojoName, specFile);
    }
    return pojoName;
  }

  public static String getDateType(final JsonNode schema, final CommonSpecFile specFile) {
    return switch (ApiTool.getFormat(schema)) {
      case "date" -> switch (specFile.getUseTimeType()) {
        case ZONED -> TypeConstants.ZONEDDATE;
        case OFFSET -> TypeConstants.OFFSETDATE;
        default -> TypeConstants.LOCALDATE;
      };
      case "date-time" -> switch (specFile.getUseTimeType()) {
        case ZONED -> TypeConstants.ZONEDDATETIME;
        case OFFSET -> TypeConstants.OFFSETDATETIME;
        default -> TypeConstants.LOCALDATETIME;
      };
      default -> TypeConstants.LOCALDATETIME;
    };
  }

  public static String getRefClass(final JsonNode schema) {
    final String[] pathObjectRef = getStrings(schema);
    return pathObjectRef[pathObjectRef.length - 1];
  }

  private static String[] getStrings(final JsonNode schema) {
    return splitName(schema.get(REF).textValue());
  }

  public static String[] splitName(final String name) {
    return ArrayUtils.removeAllOccurrences(name.split("\\W+"), "");
  }

  public static String getLongRefClass(final JsonNode schema) {
    final String[] pathObjectRef = getStrings(schema);
    return pathObjectRef[pathObjectRef.length - 2] + "/" + pathObjectRef[pathObjectRef.length - 1];
  }

  public static String getNameFromFile(final String filePath) {
    return capitalizeFileName(StringUtils
                                  .removeStart(filePath, "./")
                                  .substring(0, filePath.lastIndexOf('.') - 2)
                                  .replace("\\/", "."));
  }

  public static String capitalizeFileName(final String name) {
    final StringBuilder response = new StringBuilder();
    if (StringUtils.containsAny(name, DIVISOR)) {
      final var splitPackage = MapperUtil.splitName(name);
      for (String s : splitPackage) {
        response.append(StringUtils.capitalize(s));
      }
    } else {
      response.append(StringUtils.capitalize(name));
    }
    return response.toString();
  }

  public static String capitalizeWithPrefix(final String name) {
    final StringBuilder response = new StringBuilder();
    if (StringUtils.containsAny(name, DIVISOR)) {
      final var splitPackage = MapperUtil.splitName(name);
      for (int i = 0; i < splitPackage.length; i++) {
        response.append(PACKAGE_SEPARATOR_STR).append(i < splitPackage.length - 1 ? splitPackage[i] : StringUtils.capitalize(splitPackage[i]));
      }
    } else {
      response.append(PACKAGE_SEPARATOR_STR).append(StringUtils.capitalize(name));
    }
    return response.toString();
  }

  public static String getModel(String reference) {
    return reference.substring(reference.lastIndexOf("/") + 1);
  }

  public static String getPathToModel(String reference) {
    return reference.substring(calculateStart(reference), reference.lastIndexOf("/"));
  }

  private static int calculateStart(final String reference) {
    return StringUtils.startsWith(reference, "#") ? 1 : 0;
  }
}
