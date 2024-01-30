package com.sngular.api.generator.plugin.common.util;

import static com.sngular.api.generator.plugin.common.tools.ApiTool.ALL_OF;
import static com.sngular.api.generator.plugin.common.tools.ApiTool.ANY_OF;
import static com.sngular.api.generator.plugin.common.tools.ApiTool.ONE_OF;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.ARRAY;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.BIG_DECIMAL;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.LOCAL_DATE;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.LOCAL_DATE_TIME;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.MAP;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.OBJECT;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.PROPERTIES;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.REF;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.TYPE;
import static com.sngular.api.generator.plugin.common.util.SpecificationPropertyFields.ZONED_DATE_TIME;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.asyncapi.exception.BadDefinedEnumException;
import com.sngular.api.generator.plugin.asyncapi.exception.NonSupportedSchemaException;
import com.sngular.api.generator.plugin.asyncapi.model.SchemaFieldObjectProperties;
import com.sngular.api.generator.plugin.asyncapi.util.MapperUtil;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.model.TimeType;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.common.parameter.OperationParameter;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.function.Consumer;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;

public class MapperContentUtil {

  private static String schemaCombinatorType;

  public static Map<String, SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas,
      final String schemaName,
      final JsonNode schema,
      OperationParameter operationParameter) {
    if (Objects.nonNull(schema)) {
      final Queue<String> modelToBuildList = new ConcurrentLinkedQueue<>();
      String modelPackage = operationParameter.getModelPackage();
      String parentPackage = modelPackage.substring(modelPackage.lastIndexOf(".") + 1);
      SchemaObject builtSchemaObject =
          buildSchemaObject(
              totalSchemas,
              schemaName,
              schema,
              operationParameter,
              modelToBuildList,
              parentPackage);
      compositedSchemas.put(builtSchemaObject.getSchemaName(), builtSchemaObject);
      while (!modelToBuildList.isEmpty()) {
        final var modelToBuild = modelToBuildList.remove();
        if (!compositedSchemas.containsKey(modelToBuild)) {
          final var path = MapperUtil.splitName(modelToBuild);
          final var nexElement =
              buildSchemaObject(
                  totalSchemas,
                  modelToBuild,
                  totalSchemas.get(getComponent(path)),
                  operationParameter,
                  modelToBuildList,
                  getParentName(path));

          if (compositedSchemas.containsKey(nexElement.getSchemaName())) {
            modelToBuildList.poll();
          }
          compositedSchemas.put(nexElement.getSchemaName(), nexElement);
        }
      }
    }
    return compositedSchemas;
  }

  private static String getParentName(final String[] path) {
    final String parenName;
    if (path.length > 1) {
      parenName = path[path.length - 2];
    } else {
      parenName = "";
    }
    return parenName;
  }

  private static String getComponent(final String[] path) {
    final String componentName;
    if (path.length > 1) {
      componentName = path[path.length - 1];
    } else {
      componentName = path[0];
    }
    return componentName;
  }

  private static SchemaObject buildSchemaObject(
      final Map<String, JsonNode> totalSchemas,
      final String component,
      final JsonNode model,
      final OperationParameter operationParameter,
      final Collection<String> modelToBuildList,
      final String parentPackage) {

    final var listSchema =
        getFields(totalSchemas, model, true, operationParameter, modelToBuildList, parentPackage);
    final var splitPackage = MapperUtil.splitName(component);
    final String className = splitPackage[splitPackage.length - 1];
    final String prefix = operationParameter.getModelNamePrefix();
    final String suffix = operationParameter.getModelNameSuffix();
    return SchemaObject.builder()
        .schemaName(WordUtils.capitalizeFully(className))
        .className(MapperUtil.getPojoName(className, prefix, suffix))
        .importList(getImportList(listSchema))
        .schemaCombinator(StringUtils.isNotBlank(schemaCombinatorType) ? schemaCombinatorType : "")
        .fieldObjectList(listSchema)
        .parentPackage(parentPackage.toLowerCase())
        .build();
  }

  private static List<String> getImportList(final List<SchemaFieldObject> schemaListToImport) {
    final var importList = new HashSet<String>();

    for (SchemaFieldObject fieldObject : schemaListToImport) {
      importList.addAll(getTypeImports(fieldObject));
    }
    return new ArrayList<>(importList);
  }

  private static List<String> getTypeImports(final SchemaFieldObject fieldObject) {
    final List<String> importList = new ArrayList<>();
    if (Objects.nonNull(fieldObject.getDataTypeSimple())) {
      if (fieldObject.getDataTypeSimple().equals(ARRAY)) {
        importList.addAll(List.of("java.util.List", "java.util.ArrayList"));
        if (Objects.nonNull(fieldObject.getDataType())) {
          if (Objects.equals(fieldObject.getDataType(), LOCAL_DATE)) {
            importList.add("java.time.LocalDate");
          } else if (Objects.equals(fieldObject.getDataType(), LOCAL_DATE_TIME)) {
            importList.add("java.time.LocalDateTime");
          } else if (Objects.equals(fieldObject.getDataType(), ZONED_DATE_TIME)) {
            importList.add("java.time.ZonedDateTime");
          }
        }
      } else if (Objects.equals(fieldObject.getDataTypeSimple(), MAP)) {
        importList.addAll(List.of("java.util.Map", "java.util.HashMap"));
      } else if (fieldObject.getDataTypeSimple().equals(BIG_DECIMAL)
          || Objects.nonNull(fieldObject.getDataType())
              && fieldObject.getDataType().equals(BIG_DECIMAL)) {
        importList.add("java.math.BigDecimal");
      } else if (Objects.equals(fieldObject.getDataTypeSimple(), LOCAL_DATE)) {
        importList.add("java.time.LocalDate");
      } else if (Objects.equals(fieldObject.getDataTypeSimple(), LOCAL_DATE_TIME)) {
        importList.add("java.time.LocalDateTime");
      } else if (Objects.equals(fieldObject.getDataTypeSimple(), ZONED_DATE_TIME)) {
        importList.add("java.time.ZonedDateTime");
      }
    }
    return importList;
  }

  private static List<SchemaFieldObject> getFields(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode model,
      final boolean required,
      final OperationParameter operationParameter,
      final Collection<String> modelToBuildList,
      final String parentPackage) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    schemaCombinatorType = null;
    if (ApiTool.hasType(model)) {
      String objectType = model.get(TYPE).textValue();
      if (OBJECT.equalsIgnoreCase(objectType)) {
        fieldObjectArrayList.addAll(
            processFieldObject(
                totalSchemas, model, operationParameter, modelToBuildList, parentPackage));
      } else if (ARRAY.equalsIgnoreCase(objectType)) {
        fieldObjectArrayList.add(
            processFieldObjectList(
                totalSchemas,
                "",
                model,
                required,
                operationParameter,
                modelToBuildList,
                parentPackage,
                null));
      } else if ("enum".equalsIgnoreCase(objectType)) {
        fieldObjectArrayList.add(
            processFieldObjectList(
                totalSchemas,
                "",
                model,
                required,
                operationParameter,
                modelToBuildList,
                parentPackage,
                null));
      }
    } else if (ApiTool.isAllOf(model)) {
      fieldObjectArrayList.addAll(
          processAllOf(
              totalSchemas, ApiTool.getAllOf(model), operationParameter, modelToBuildList));
    } else if (ApiTool.hasRef(model)) {
      final var splitName = MapperUtil.splitName(ApiTool.getRefValue(model));
      fieldObjectArrayList.addAll(
          processFieldObject(
              totalSchemas,
              totalSchemas.get(MapperUtil.buildKey(splitName)),
              operationParameter,
              modelToBuildList,
              parentPackage));
    } else if (model.elements().hasNext()) {
      fieldObjectArrayList.addAll(
          processFieldObject(
              totalSchemas, model, operationParameter, modelToBuildList, parentPackage));
    }
    return fieldObjectArrayList;
  }

  private static Set<SchemaFieldObject> processAllOf(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode schemaList,
      final OperationParameter specFile,
      final Map<String, SchemaObject> compositedSchemas) {
    final Set<SchemaFieldObject> fieldObjectArrayList = new HashSet<>();

    for (JsonNode ref : schemaList) {
      if (ApiTool.hasRef(ref)) {
        final String schemaName = ApiTool.getRefValue(ref);
        final var schemaToProcess = totalSchemas.get(schemaName);
        ApiTool.getProperties(schemaToProcess)
            .forEachRemaining(
                processProperties(
                    schemaName,
                    totalSchemas,
                    compositedSchemas,
                    fieldObjectArrayList,
                    specFile,
                    ref));
        for (var fieldObject : fieldObjectArrayList) {
          fieldObject.setRequired(true);
        }
      }
    }
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject processEnumField(
      final String key,
      final JsonNode value,
      final OperationParameter operationParameter,
      final List<String> enumValues,
      final JsonNode schema) {
    final var field =
        SchemaFieldObject.builder()
            .baseName(key)
            .dataType(new SchemaFieldObjectType(TypeConstants.ENUM))
            .build();
    field.setRequired(ApiTool.checkIfRequired(schema, key));
    final var dataType =
        com.sngular.api.generator.plugin.openapi.utils.MapperUtil.getSimpleType(
            value, operationParameter);
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
    field.setEnumValues(new ArrayList<>(enumValuesMap.values()));
    return field;
  }

  private static Consumer<Entry<String, JsonNode>> processProperties(
      final String buildingSchema,
      final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas,
      final Set<SchemaFieldObject> fieldObjectArrayList,
      final OperationParameter operationParameter,
      final JsonNode schema) {
    return field -> {
      final var nodeName = field.getKey();
      final var nodeValue = field.getValue();
      if (ApiTool.isEnum(field.getValue())) {
        fieldObjectArrayList.add(
            processEnumField(
                nodeName, nodeValue, operationParameter, ApiTool.getEnumValues(nodeValue), schema));
      } else {
        fieldObjectArrayList.addAll(
            processObjectProperty(
                buildingSchema,
                totalSchemas,
                nodeName,
                nodeValue,
                compositedSchemas,
                operationParameter,
                schema));
      }
    };
  }

  private static List<SchemaFieldObject> processFieldObject(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode model,
      final OperationParameter operationParameter,
      final Collection<String> modelToBuildList,
      final String parentPackage) {
    final Set<String> requiredSet = new HashSet<>();
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    if (model.has("required")) {
      final JsonNode arrayNode = model.get("required");
      final Iterator<JsonNode> fields = arrayNode.iterator();
      fields.forEachRemaining(field -> requiredSet.add(field.textValue()));
    }
    final var properties = model.get(PROPERTIES);
    if (Objects.nonNull(properties)) {
      if (!(properties.has(ANY_OF) || properties.has(ALL_OF) || properties.has(ONE_OF))) {
        final var propertiesIt = model.get(PROPERTIES).fieldNames();
        while (propertiesIt.hasNext()) {
          final var property = propertiesIt.next();
          fieldObjectArrayList.add(
              processFieldObjectList(
                  totalSchemas,
                  property,
                  model.get(PROPERTIES).path(property),
                  requiredSet.contains(property),
                  operationParameter,
                  modelToBuildList,
                  parentPackage,
                  null));
          if (model.get(PROPERTIES).path(property).has(REF)
              && !totalSchemas.containsKey(createKey(parentPackage, property.toUpperCase(), "/"))) {
            modelToBuildList.add(MapperUtil.getSortRefClass(model.get(PROPERTIES).path(property)));
          }
        }
      } else if (properties.has(ALL_OF)) {
        fieldObjectArrayList.addAll(
            processAllOfAnyOfOneOf(
                totalSchemas,
                properties.get(ALL_OF),
                true,
                operationParameter,
                modelToBuildList,
                parentPackage));
        schemaCombinatorType = ALL_OF;
      } else if (properties.has(ANY_OF)) {
        fieldObjectArrayList.addAll(
            processAllOfAnyOfOneOf(
                totalSchemas,
                properties.get(ANY_OF),
                false,
                operationParameter,
                modelToBuildList,
                parentPackage));
        schemaCombinatorType = ANY_OF;
      } else if (properties.has(ONE_OF)) {
        fieldObjectArrayList.addAll(
            processAllOfAnyOfOneOf(
                totalSchemas,
                properties.get(ONE_OF),
                false,
                operationParameter,
                modelToBuildList,
                parentPackage));
        schemaCombinatorType = ONE_OF;
      }
    }

    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAllOfAnyOfOneOf(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode schemaList,
      final boolean required,
      final OperationParameter operationParameter,
      final Collection<String> modelToBuildList,
      final String parentPackage) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    final var allOfIterator = schemaList.elements();

    allOfIterator.forEachRemaining(
        element ->
            fieldObjectArrayList.add(
                solveElement(
                    totalSchemas,
                    required,
                    operationParameter,
                    element,
                    modelToBuildList,
                    parentPackage)));
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject solveElement(
      final Map<String, JsonNode> totalSchemas,
      final boolean required,
      final OperationParameter operationParameter,
      final JsonNode element,
      final Collection<String> modelToBuildList,
      final String parentPackage) {
    final SchemaFieldObject result;
    if (element.has(REF)) {
      final String schemaName = MapperUtil.getSortRefClass(element);
      final var schemaToProcess = totalSchemas.get(schemaName);
      result =
          processFieldObjectList(
              totalSchemas,
              schemaName,
              schemaToProcess,
              required,
              operationParameter,
              modelToBuildList,
              parentPackage,
              null);
      result.setRequired(false);
    } else {
      result =
          processFieldObjectList(
              totalSchemas,
              "",
              element,
              required,
              operationParameter,
              modelToBuildList,
              parentPackage,
              null);
    }
    return result;
  }

  private static SchemaFieldObject processFieldObjectList(
      final Map<String, JsonNode> totalSchemas,
      final String propertyName,
      final JsonNode schema,
      final boolean required,
      final OperationParameter operationParameter,
      final Collection<String> modelToBuildList,
      final String modelPackage,
      final String className) {
    final SchemaFieldObject fieldObject;
    final var name = schema.has("name") ? schema.get("name").textValue() : propertyName;
    final String prefix = operationParameter.getModelNamePrefix();
    final String suffix = operationParameter.getModelNameSuffix();
    final TimeType useTimeType = operationParameter.getUseTimeType();
    final Map<String, String> formats = operationParameter.getFormats();

    String simpleType = MapperUtil.getSimpleType(schema, prefix, suffix, useTimeType);
    if (ApiTool.hasType(schema)) {
      final var type = ApiTool.getType(schema);
      if (OBJECT.equalsIgnoreCase(type)) {
        fieldObject =
            SchemaFieldObject.builder()
                .baseName(name)
                .restrictions(new SchemaFieldObjectProperties())
                .dataType(new SchemaFieldObjectType(simpleType))
                .build();
        setFieldType(
            fieldObject, schema, required, prefix, suffix, className, formats, useTimeType);
        final var schemaName = StringUtils.defaultString(className, propertyName);
        if (StringUtils.isNotEmpty(schemaName)
            && !totalSchemas.containsKey(createKey(modelPackage, schemaName.toUpperCase(), "/"))) {
          totalSchemas.put(schemaName, schema);
          modelToBuildList.add(createKey(modelPackage.toLowerCase(), schemaName, "."));
        }
      } else if (ApiTool.hasItems(schema)) {
        final var items = ApiTool.getItems(schema);
        final var arrayType = MapperUtil.getSimpleType(items, prefix, suffix, useTimeType);
        if (items.has(REF)) {
          final var longType = MapperUtil.getSortRefClass(items);
          modelToBuildList.add(longType);
        }
        fieldObject =
            SchemaFieldObject.builder()
                .baseName(name)
                .restrictions(new SchemaFieldObjectProperties())
                .dataType(new SchemaFieldObjectType(arrayType))
                .dataTypeSimple(type)
                .importClass(getImportClass(arrayType))
                .build();
        setFormatProperies(fieldObject, arrayType, formats);
        handleItems(schema, modelToBuildList, fieldObject, required, items);
      } else if (ApiTool.isEnum(schema)) {
        fieldObject = processEnumField(name, required, schema, prefix, suffix, useTimeType);
      } else {
        fieldObject =
            SchemaFieldObject.builder()
                .baseName(name)
                .restrictions(new SchemaFieldObjectProperties())
                .dataType(new SchemaFieldObjectType(simpleType))
                .dataTypeSimple(simpleType)
                .constValue(MapperUtil.getConstValue(schema))
                .build();
        setFieldProperties(fieldObject, schema);
        setFormatProperies(fieldObject, simpleType, formats);
        fieldObject.setRequired(required);
      }
    } else if (ApiTool.hasRef(schema)) {
      final var splitName = MapperUtil.splitName(ApiTool.getRefValue(schema));
      final var solvedRef = totalSchemas.get(getComponent(splitName));
      fieldObject =
          processFieldObjectList(
              totalSchemas,
              name,
              solvedRef,
              required,
              operationParameter,
              modelToBuildList,
              modelPackage,
              splitName[splitName.length - 1]);
    } else {
      fieldObject =
          SchemaFieldObject.builder()
              .baseName(name)
              .dataType(new SchemaFieldObjectType(simpleType))
              .dataTypeSimple(simpleType)
              .restrictions(new SchemaFieldObjectProperties())
              .constValue(MapperUtil.getConstValue(schema))
              .build();
    }
    return fieldObject;
  }

  private static String createKey(
      final String modelPackage, final String className, final String separator) {
    return Objects.nonNull(modelPackage) ? modelPackage + separator + className : className;
  }

  private static void handleItems(
      final JsonNode schema,
      final Collection<String> modelToBuildList,
      final SchemaFieldObject fieldObject,
      final boolean required,
      final JsonNode items) {
    if (ApiTool.hasRef(items)) {
      modelToBuildList.add(MapperUtil.getSortRefClass(items));
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

  @SuppressWarnings("checkstyle:CyclomaticComplexity")
  private static void setFieldProperties(
      final SchemaFieldObject fieldObject, final JsonNode schema) {
    final Iterator<Map.Entry<String, JsonNode>> iterator = schema.fields();
    Entry<String, JsonNode> current;
    final SchemaFieldObjectProperties props = fieldObject.getRestrictions();
    while (iterator.hasNext()) {
      current = iterator.next();
      switch (current.getKey()) {
        case "minimum":
          props.setMinimum(current.getValue().asText());
          break;
        case "maximum":
          props.setMaximum(current.getValue().asText());
          break;
        case "exclusiveMinimum":
          props.setExclusiveMinimum(current.getValue().booleanValue());
          break;
        case "exclusiveMaximum":
          props.setExclusiveMaximum(current.getValue().booleanValue());
          break;
        case "maxItems":
          props.setMaxItems(current.getValue().intValue());
          break;
        case "maxLength":
          props.setMaxLength(current.getValue().intValue());
          break;
        case "minItems":
          props.setMinItems(current.getValue().intValue());
          break;
        case "minLength":
          props.setMinLength(current.getValue().intValue());
          break;
        case "pattern":
          props.setPattern(current.getValue().toString().replace("\"", ""));
          break;
        case "uniqueItems":
          props.setUniqueItems(current.getValue().booleanValue());
          break;
        case "multipleOf":
          props.setMultipleOf(current.getValue().asText());
          break;
        default:
          break;
      }
    }
  }

  private static void setFormatProperies(
      final SchemaFieldObject fieldObject,
      final String dataType,
      final Map<String, String> formats) {
    if (Objects.equals(dataType, LOCAL_DATE)) {
      fieldObject.getRestrictions().setFormat(formats.get("DATE"));
    } else if (Objects.equals(dataType, LOCAL_DATE_TIME)) {
      fieldObject.getRestrictions().setFormat(formats.get("DATE_TIME"));
    }
  }

  private static void setFieldType(
      final SchemaFieldObject field,
      final JsonNode value,
      final boolean required,
      final String prefix,
      final String suffix,
      final String className,
      final Map<String, String> formats,
      final TimeType useTimeType) {
    field.setRequired(required);
    if (ApiTool.hasType(value)) {
      if (ARRAY.equalsIgnoreCase(ApiTool.getType(value))) {
        final var typeArray = MapperUtil.getTypeArray(value, prefix, suffix, useTimeType);
        field.setDataType(new SchemaFieldObjectType(typeArray));
        field.setImportClass(getImportClass(typeArray));
        setFormatProperies(field, typeArray, formats);
      } else if (ApiTool.getType(value).equalsIgnoreCase(OBJECT)) {
        if (value.has("additionalProperties")) {
          final var typeMap = MapperUtil.getTypeMap(value, prefix, suffix, useTimeType);
          field.setDataTypeSimple(MAP);
          field.setDataType(new SchemaFieldObjectType(typeMap));
          field.setImportClass(getImportClass(typeMap));
        } else {
          var typeObject = "";
          if (ApiTool.hasRef(value)) {
            typeObject = MapperUtil.getRef(value, prefix, suffix);
          } else {
            if (StringUtils.isEmpty(className)) {
              typeObject = MapperUtil.getPojoName(field.getBaseName(), prefix, suffix);
            } else {
              typeObject = MapperUtil.getPojoName(className, prefix, suffix);
            }
          }
          field.setImportClass(getImportClass(typeObject));
          field.setDataType(new SchemaFieldObjectType(typeObject));
          field.setDataTypeSimple(typeObject);
        }
      } else {
        throw new NonSupportedSchemaException(value.toPrettyString());
      }
    }
  }

  private static SchemaFieldObject processEnumField(
      final String name,
      final boolean required,
      final JsonNode value,
      final String prefix,
      final String suffix,
      final TimeType useTimeType) {
    final List<String> enumValues = new ArrayList<>();
    value
        .get("enum")
        .elements()
        .forEachRemaining(enumValue -> enumValues.add(enumValue.textValue()));

    if (enumValues.isEmpty()) {
      throw new BadDefinedEnumException(name);
    }
    final String simpleType = MapperUtil.getSimpleType(value, prefix, suffix, useTimeType);
    return SchemaFieldObject.builder()
        .baseName(name)
        .dataTypeSimple("enum")
        .dataType(new SchemaFieldObjectType(simpleType))
        .required(required)
        .enumValues(enumValues)
        .restrictions(new SchemaFieldObjectProperties())
        .build();
  }

  private static String getImportClass(final String type) {
    return StringUtils.isNotBlank(type) && !"String".equals(type) && !"Integer".equals(type)
        ? type
        : "";
  }
}
