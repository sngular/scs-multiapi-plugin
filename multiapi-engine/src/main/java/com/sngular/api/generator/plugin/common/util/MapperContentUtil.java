package com.sngular.api.generator.plugin.common.util;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedSchemaException;
import com.sngular.api.generator.plugin.common.model.TimeType;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.common.parameter.SpecFile;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.openapi.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.openapi.model.SchemaObject;
import com.sngular.api.generator.plugin.openapi.utils.MapperUtil;
import org.apache.commons.lang3.StringUtils;

public final class MapperContentUtil {

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode schema,
      final String schemaFullName,
      final SpecFile specFile,
      final Path baseDir) {
    final Set<String> processedSchemaNames = new HashSet<>();
    String name = StringUtils.defaultIfEmpty(ApiTool.getName(schema), schemaFullName);
    return null;
  }

  private static void setFieldType(
      final com.sngular.api.generator.plugin.asyncapi.model.SchemaFieldObject field,
      final JsonNode schemaProperty,
      final boolean required,
      final String prefix,
      final String suffix,
      final String className,
      final Map<String, String> formats,
      final TimeType useTimeType) {
    field.setRequired(required);
    if (ApiTool.hasType(value)) {
      if (ApiTool.isArray(schemaProperty)) {
        final var typeArray = com.sngular.api.generator.plugin.asyncapi.util.MapperUtil.getTypeArray(value, prefix, suffix, useTimeType);
        field.setDataType(typeArray);
        field.setImportClass(getImportClass(typeArray));
        setFormatProperies(field, typeArray, formats);
      } else if (ApiTool.hasAdditionalProperties(schemaProperty)) {
        final String typeObject = getMapTypeObject(schemaProperty, specFile);
        field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, typeObject));
        field.setImportClass(getImportClass(typeObject));

      } else if (ApiTool.isObject(schemaProperty)) {
        var typeObject = "";
        if (ApiTool.hasRef(schemaProperty)) {
          typeObject = com.sngular.api.generator.plugin.asyncapi.util.MapperUtil.getRef(schemaProperty, prefix, suffix);
        } else {
          if (StringUtils.isEmpty(className)) {
            typeObject = com.sngular.api.generator.plugin.asyncapi.util.MapperUtil.getPojoName(field.getBaseName(), prefix, suffix);
          } else {
            typeObject = com.sngular.api.generator.plugin.asyncapi.util.MapperUtil.getPojoName(className, prefix, suffix);
          }
        }
        field.setImportClass(getImportClass(typeObject));
        field.setDataType(typeObject);
        field.setDataTypeSimple(typeObject);
      } else {
        throw new NonSupportedSchemaException(schemaProperty.toPrettyString());
      }
    }
  }
  
  private static SchemaFieldObject processEnumField(
      final String key,
      final JsonNode value,
      final SpecFile specFile,
      final List<String> enumValues,
      final JsonNode schema) {
    final var field =
        SchemaFieldObject.builder()
                         .baseName(key)
                         .dataType(new SchemaFieldObjectType(TypeConstants.ENUM))
                         .build();
    field.setRequired(ApiTool.checkIfRequired(schema, key));
    final var dataType = MapperUtil.getSimpleType(value, specFile);
    field.getDataType().setDeepType(dataType);

    final HashMap<String, String> enumValuesMap = new HashMap<>();

    for (final var enumValue : enumValues) {
      String valueName = enumValue;
      valueName = valueName.replace(".", "_DOT_");

      switch (dataType) {
        case TypeConstants.INTEGER:
          enumValuesMap.put("INTEGER_" + valueName, enumValue);
          break;
        case TypeConstants.LONG:
          enumValuesMap.put("LONG_" + valueName, enumValue + "l");
          break;
        case TypeConstants.DOUBLE:
          enumValuesMap.put("DOUBLE_" + valueName, enumValue);
          break;
        case TypeConstants.FLOAT:
          enumValuesMap.put("FLOAT_" + valueName, enumValue + "f");
          break;
        case TypeConstants.BIG_DECIMAL:
          enumValuesMap.put("BIG_DECIMAL_" + valueName, "new BigDecimal(\"" + enumValue + "\")");
          break;
        case TypeConstants.STRING:
        default:
          enumValuesMap.put(StringUtils.upperCase(valueName), '"' + enumValue + '"');
          break;
      }
    }

    if (enumValuesMap.isEmpty()) {
      throw new BadDefinedEnumException(key);
    }
    field.setEnumValues(enumValuesMap);
    return field;
  }
  
  public static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !TypeConstants.NO_IMPORT_TYPE.contains(type)
               ? StringUtils.capitalize(type)
               : "";
  }
}
