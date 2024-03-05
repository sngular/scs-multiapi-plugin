package com.sngular.api.generator.plugin.common.util;

import java.nio.file.Path;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedSchemaException;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectProperties;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.model.TimeType;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.common.parameter.AbstractSpecFile;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.openapi.exception.BadDefinedEnumException;
import com.sngular.api.generator.plugin.openapi.exception.CodeGenerationException;
import com.sngular.api.generator.plugin.openapi.parameter.OpenAPIAbstractSpecFile;
import org.apache.commons.lang3.StringUtils;

public final class MapperContentUtil {

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas,
      final JsonNode schema,
      final String schemaName,
      final AbstractSpecFile specFile,
      final Path baseDir,
      final TimeType useTimeType) {
    final Set<String> antiLoopList = new HashSet<>();
    String name = StringUtils.defaultIfEmpty(ApiTool.getName(schema), schemaName);
    return null;
  }

  private static void setFieldType(
      final SchemaFieldObject field,
      final JsonNode schemaProperty,
      final boolean required,
      final AbstractSpecFile specFile,
      final String className,
      final Map<String, String> formats,
      final TimeType useTimeType) {
    field.setRequired(required);
    if (ApiTool.hasType(schemaProperty)) {
      field.setImportClass(getImportClass(typeArray));
      field.setDataType(getSchemaType(schemaProperty, field.getBaseName(), specFile, globalObject, baseDir));
      if (ApiTool.isArray(schemaProperty)) {
        final var typeArray = com.sngular.api.generator.plugin.common.util.MapperUtil.getTypeArray(schemaProperty, specFile, useTimeType);
        setFormatProperties(field, typeArray, formats);
      } else if (ApiTool.hasAdditionalProperties(schemaProperty)) {
        final String typeObject = getMapTypeObject(schemaProperty, specFile);
        field.setDataType(SchemaFieldObjectType.fromTypeList(TypeConstants.MAP, typeObject));
        field.setImportClass(getImportClass(typeObject));

      } else if (ApiTool.isObject(schemaProperty)) {
        var typeObject = "";
        if (ApiTool.hasRef(schemaProperty)) {
          typeObject = MapperUtil.getRef(schemaProperty, prefix, suffix);
        } else {
          if (StringUtils.isEmpty(className)) {
            typeObject = MapperUtil.getPojoName(field.getBaseName(), prefix, suffix);
          } else {
            typeObject = MapperUtil.getPojoName(className, prefix, suffix);
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

  private static String getMapTypeObject(final JsonNode schema, final OpenAPIAbstractSpecFile specFile) {
    final String type;
    if (ApiTool.isBoolean(ApiTool.getAdditionalProperties(schema))) {
      type = TypeConstants.OBJECT;
    } else {
      final JsonNode additionalProperties = ApiTool.getAdditionalProperties(schema);
      if (ApiTool.hasRef(additionalProperties)) {
        type = MapperUtil.getRef(additionalProperties, specFile);
      } else if (ApiTool.isObject(schema)) {
        final var additionalPropertiesField =
            SchemaFieldObject.builder()
                             .baseName(ApiTool.getName(additionalProperties))
                             .dataType(
                                 new SchemaFieldObjectType(
                                     MapperUtil.getSimpleType(additionalProperties, specFile)))
                             .build();
        setFieldType(
            additionalPropertiesField, additionalProperties, additionalProperties, specFile, "");
        type = getMapFieldType(additionalPropertiesField);
      } else {
        type = TypeConstants.OBJECT;
      }
    }

    return type;
  }
  
  private static void handleItems(
      final JsonNode schema,
      final Collection<String> modelToBuildList,
      final SchemaFieldObject fieldObject,
      final boolean required,
      final JsonNode items) {
    if (ApiTool.hasRef(items)) {
      modelToBuildList.add(MapperUtil.getLongRefClass(items));
    }
    final Iterator<Map.Entry<String, JsonNode>> iterator = schema.fields();
    Entry<String, JsonNode> current;
    while (iterator.hasNext()) {
      current = iterator.next();
      switch (current.getKey()) {
        case "maxItems":
          fieldObject.getRestrictions().setMaxItems(current.getValue().intValue());
          break;
        case "minItems":
          fieldObject.getRestrictions().setMinItems(current.getValue().intValue());
          break;
        case "uniqueItems":
          fieldObject.getRestrictions().setUniqueItems(current.getValue().booleanValue());
          break;
        default:
          break;
      }
    }
    fieldObject.setRequired(required);
  }


  private static void setFieldProperties(
      final SchemaFieldObject fieldObject, final JsonNode schema) {
    final Iterator<Entry<String, JsonNode>> iterator = schema.fields();
    Entry<String, JsonNode> current;
    final SchemaFieldObjectProperties props = fieldObject.getRestrictions();
    while (iterator.hasNext()) {
      current = iterator.next();
      switch (current.getKey()) {
        case "minimum" -> props.setMinimum(current.getValue().asText());
        case "maximum" -> props.setMaximum(current.getValue().asText());
        case "exclusiveMinimum" -> props.setExclusiveMinimum(current.getValue().booleanValue());
        case "exclusiveMaximum" -> props.setExclusiveMaximum(current.getValue().booleanValue());
        case "maxItems" -> props.setMaxItems(current.getValue().intValue());
        case "maxLength" -> props.setMaxLength(current.getValue().intValue());
        case "minItems" -> props.setMinItems(current.getValue().intValue());
        case "minLength" -> props.setMinLength(current.getValue().intValue());
        case "pattern" -> props.setPattern(current.getValue().toString().replace("\"", ""));
        case "uniqueItems" -> props.setUniqueItems(current.getValue().booleanValue());
        case "multipleOf" -> props.setMultipleOf(current.getValue().asText());
        default () -> throw new CodeGenerationException("Unknown Property " + current.getKey());
      }
    }
  }

  private static void setFormatProperties(
      final SchemaFieldObject fieldObject,
      final String dataType,
      final Map<String, String> formats) {
    if (Objects.equals(dataType, SpecificationPropertyFields.LOCAL_DATE)) {
      fieldObject.getRestrictions().setFormat(formats.getOrDefault("DATE", null));
    } else if (Objects.equals(dataType, SpecificationPropertyFields. LOCAL_DATE_TIME)) {
      fieldObject.getRestrictions().setFormat(formats.getOrDefault("DATE_TIME", null));
    }
  }


  private static String getMapFieldType(final SchemaFieldObject schemaFieldObject) {
    final String fieldType = schemaFieldObject.getDataType().toString();

    return switch (fieldType) {
      case TypeConstants.BIG_DECIMAL, TypeConstants.INTEGER, TypeConstants.DOUBLE, TypeConstants.FLOAT, TypeConstants.LONG, TypeConstants.STRING -> fieldType;
      default -> TypeConstants.OBJECT;
    };
  }
  
  private static SchemaFieldObject processEnumField(
      final String key,
      final JsonNode value,
      final AbstractSpecFile specFile,
      final List<String> enumValues,
      final JsonNode schema,
      final TimeType useTimeType) {
    final var field =
        SchemaFieldObject.builder()
                         .baseName(key)
                         .dataType(new SchemaFieldObjectType(TypeConstants.ENUM))
                         .build();
    field.setRequired(ApiTool.checkIfRequired(schema, key));
    final var dataType = MapperUtil.getSimpleType(value, specFile, useTimeType);
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
