/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.util;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectProperties;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import com.sngular.api.generator.plugin.common.model.TimeType;
import com.sngular.api.generator.plugin.common.parameter.AbstractSpecFile;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.util.MapperUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;

public class MapperContentUtil {

  public static final String OBJECT = "object";

  public static final String ONE_OF = "oneOf";

  public static final String PROPERTIES = "properties";

  public static final String REF = "$ref";

  public static final String TYPE = "type";

  public static final String ALL_OF = "allOf";

  public static final String ANY_OF = "anyOf";

  private static final String ARRAY = "array";

  private static final String BIG_DECIMAL = "BigDecimal";

  private static final String LOCAL_DATE = "LocalDate";

  private static final String LOCAL_DATE_TIME = "LocalDateTime";

  private static final String ZONED_DATE_TIME = "ZonedDateTime";

  private static final String MAP = "map";

  private static String schemaCombinatorType;

  private MapperContentUtil() {}

  public static List<SchemaObject> mapComponentToSchemaObject(
      final Map<String, JsonNode> totalSchemas,
      final Map<String, SchemaObject> compositedSchemas,
      final JsonNode schema,
      final String schemaName,
      final AbstractSpecFile specFile,
      final Map<String, String> formats,
      final Path baseDir,
      final TimeType useTimeType) {
    final List<SchemaObject> schemasList = new ArrayList<>();
    if (Objects.nonNull(schema)) {
      final List<String> alreadyBuilt = new ArrayList<>();
      final Queue<String> modelToBuildList = new ConcurrentLinkedQueue<>();
      schemasList.add(
          buildSchemaObject(
              totalSchemas,
              component,
              model,
              prefix,
              suffix,
              modelToBuildList,
              parentPackage,
              formats,
              useTimeType));
      while (!modelToBuildList.isEmpty()) {

        final var modelToBuild = modelToBuildList.remove();
        if (!alreadyBuilt.contains(modelToBuild)) {
          final var path = MapperUtil.splitName(modelToBuild);
          final var nexElement =
              buildSchemaObject(
                  totalSchemas,
                  modelToBuild,
                  totalSchemas.get(getComponent(path)),
                  prefix,
                  suffix,
                  modelToBuildList,
                  getParentName(path),
                  formats,
                  useTimeType);
          if (schemasList.contains(nexElement)) {
            modelToBuildList.poll();
          } else {
            schemasList.add(nexElement);
          }
          alreadyBuilt.add(modelToBuild);
        }
      }
    }
    return schemasList;
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
      componentName = path[path.length - 2] + "/" + path[path.length - 1];
    } else {
      componentName = path[0];
    }
    return componentName.toUpperCase();
  }

  private static SchemaObject buildSchemaObject(
      final Map<String, JsonNode> totalSchemas,
      final String component,
      final JsonNode model,
      final String prefix,
      final String suffix,
      final Collection<String> modelToBuildList,
      final String parentPackage,
      final Map<String, String> formats,
      final TimeType useTimeType) {

    final var listSchema =
        getFields(
            totalSchemas,
            model,
            true,
            prefix,
            suffix,
            modelToBuildList,
            parentPackage,
            formats,
            useTimeType);
    final var splitPackage = MapperUtil.splitName(component);
    final String className = splitPackage[splitPackage.length - 1];
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

  private static Set<SchemaFieldObject> getFields(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode model,
      final boolean required,
      final String prefix,
      final String suffix,
      final Collection<String> modelToBuildList,
      final String parentPackage,
      final Map<String, String> formats,
      final TimeType useTimeType) {
    final var fieldObjectArrayList = new HashSet<>();
    schemaCombinatorType = null;
    if (ApiTool.hasType(model)) {
      final String objectType = model.get(TYPE).textValue();
      if (OBJECT.equalsIgnoreCase(objectType)) {
        fieldObjectArrayList.addAll(
            processFieldObject(
                totalSchemas,
                model,
                prefix,
                suffix,
                modelToBuildList,
                parentPackage,
                formats,
                useTimeType));
      } else if (ARRAY.equalsIgnoreCase(objectType)) {
        fieldObjectArrayList.add(
            processFieldObjectList(
                totalSchemas,
                "",
                model,
                required,
                prefix,
                suffix,
                modelToBuildList,
                parentPackage,
                null,
                formats,
                useTimeType));
      } else if ("enum".equalsIgnoreCase(objectType)) {
        fieldObjectArrayList.add(
            processFieldObjectList(
                totalSchemas,
                "",
                model,
                required,
                prefix,
                suffix,
                modelToBuildList,
                parentPackage,
                null,
                formats,
                useTimeType));
      }
    } else if (ApiTool.hasRef(model)) {
      final var splitName = MapperUtil.splitName(ApiTool.getRefValue(model));
      fieldObjectArrayList.addAll(
          processFieldObject(
              totalSchemas,
              totalSchemas.get(MapperUtil.buildKey(splitName)),
              prefix,
              suffix,
              modelToBuildList,
              parentPackage,
              formats,
              useTimeType));
    } else if (model.elements().hasNext()) {
      fieldObjectArrayList.addAll(
          processFieldObject(
              totalSchemas,
              model,
              prefix,
              suffix,
              modelToBuildList,
              parentPackage,
              formats,
              useTimeType));
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processFieldObject(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode model,
      final String prefix,
      final String suffix,
      final Collection<String> modelToBuildList,
      final String parentPackage,
      final Map<String, String> formats,
      final TimeType useTimeType) {
    final Set<String> requiredSet = new HashSet<>();
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    if (model.has("required")) {
      final JsonNode arrayNode = model.get("required");
      final Iterator<JsonNode> fields = arrayNode.iterator();
      fields.forEachRemaining(field -> requiredSet.add(field.textValue()));
    }
    final var properties = model.get(PROPERTIES);
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
                prefix,
                suffix,
                modelToBuildList,
                parentPackage,
                null,
                formats,
                useTimeType));
        if (model.get(PROPERTIES).path(property).has(REF)
            && !totalSchemas.containsKey(createKey(parentPackage, property.toUpperCase(), "/"))) {
          modelToBuildList.add(MapperUtil.getLongRefClass(model.get(PROPERTIES).path(property)));
        }
      }
    } else if (properties.has(ALL_OF)) {
      fieldObjectArrayList.addAll(
          processAllOfAnyOfOneOf(
              totalSchemas,
              properties.get(ALL_OF),
              true,
              prefix,
              suffix,
              modelToBuildList,
              formats,
              useTimeType));
      schemaCombinatorType = ALL_OF;
    } else if (properties.has(ANY_OF)) {
      fieldObjectArrayList.addAll(
          processAllOfAnyOfOneOf(
              totalSchemas,
              properties.get(ANY_OF),
              false,
              prefix,
              suffix,
              modelToBuildList,
              formats,
              useTimeType));
      schemaCombinatorType = ANY_OF;
    } else if (properties.has(ONE_OF)) {
      fieldObjectArrayList.addAll(
          processAllOfAnyOfOneOf(
              totalSchemas,
              properties.get(ONE_OF),
              false,
              prefix,
              suffix,
              modelToBuildList,
              formats,
              useTimeType));
      schemaCombinatorType = ONE_OF;
    }
    return fieldObjectArrayList;
  }

  private static List<SchemaFieldObject> processAllOfAnyOfOneOf(
      final Map<String, JsonNode> totalSchemas,
      final JsonNode schemaList,
      final boolean required,
      final String prefix,
      final String suffix,
      final Collection<String> modelToBuildList,
      final Map<String, String> formats,
      final TimeType useTimeType) {
    final var fieldObjectArrayList = new ArrayList<SchemaFieldObject>();
    final var allOfIterator = schemaList.elements();

    allOfIterator.forEachRemaining(
        element ->
            fieldObjectArrayList.add(
                solveElement(
                    totalSchemas,
                    required,
                    prefix,
                    suffix,
                    element,
                    modelToBuildList,
                    formats,
                    useTimeType)));
    return fieldObjectArrayList;
  }

  private static SchemaFieldObject solveElement(
      final Map<String, JsonNode> totalSchemas,
      final boolean required,
      final String prefix,
      final String suffix,
      final JsonNode element,
      final Collection<String> modelToBuildList,
      final Map<String, String> formats,
      final TimeType useTimeType) {
    final SchemaFieldObject result;
    if (element.has(REF)) {
      final String schemaName = MapperUtil.getLongRefClass(element);
      final var schemaToProcess = totalSchemas.get(schemaName.toUpperCase());
      result =
          processFieldObjectList(
              totalSchemas,
              schemaName,
              schemaToProcess,
              required,
              prefix,
              suffix,
              modelToBuildList,
              null,
              null,
              formats,
              useTimeType);
      result.setRequired(false);
    } else {
      result =
          processFieldObjectList(
              totalSchemas,
              "",
              element,
              required,
              prefix,
              suffix,
              modelToBuildList,
              null,
              null,
              formats,
              useTimeType);
    }
    return result;
  }

  private static SchemaFieldObject processFieldObjectList(
      final Map<String, JsonNode> totalSchemas,
      final String propertyName,
      final JsonNode schema,
      final boolean required,
      final String prefix,
      final String suffix,
      final Collection<String> modelToBuildList,
      final String modelPackage,
      final String className,
      final Map<String, String> formats,
      final TimeType useTimeType) {
    final SchemaFieldObject fieldObject;
    final var name = schema.has("name") ? schema.get("name").textValue() : propertyName;
    if (ApiTool.hasType(schema)) {
      final var type = ApiTool.getType(schema);
      if (OBJECT.equalsIgnoreCase(type)) {
        fieldObject =
            SchemaFieldObject.builder()
                             .baseName(name)
                             .restrictions(new SchemaFieldObjectProperties())
                             .dataType(MapperUtil.getSimpleType(schema, prefix, suffix, useTimeType))
                             .build();
        setFieldType(
            fieldObject, schema, required, prefix, suffix, className, formats, useTimeType);
        final var schemaName = StringUtils.defaultString(className, propertyName);
        if (StringUtils.isNotEmpty(schemaName)
            && !totalSchemas.containsKey(createKey(modelPackage, schemaName.toUpperCase(), "/"))) {
          totalSchemas.put(createKey(modelPackage, schemaName.toUpperCase(), "/"), schema);
          modelToBuildList.add(createKey(modelPackage.toLowerCase(), schemaName, "."));
        }
      } else if (ApiTool.hasItems(schema)) {
        final var items = ApiTool.getItems(schema);
        final var arrayType = MapperUtil.getSimpleType(items, prefix, suffix, useTimeType);
        if (items.has(REF)) {
          final var longType = MapperUtil.getLongRefClass(items);
          modelToBuildList.add(longType);
        }
        fieldObject =
            SchemaFieldObject.builder()
                             .baseName(name)
                             .restrictions(new SchemaFieldObjectProperties())
                             .dataType(arrayType)
                             .dataTypeSimple(type)
                             .importClass(getImportClass(arrayType))
                             .build();
        setFormatProperies(fieldObject, arrayType, formats);
        handleItems(schema, modelToBuildList, fieldObject, required, items);
      } else if (ApiTool.isEnum(schema)) {
        fieldObject = processEnumField(name, required, schema, prefix, suffix, useTimeType);
      } else {
        final String simpleType = MapperUtil.getSimpleType(schema, prefix, suffix, useTimeType);
        fieldObject = SchemaFieldObject
                          .builder()
                          .baseName(name)
                          .restrictions(new SchemaFieldObjectProperties())
                          .dataType(simpleType)
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
              prefix,
              suffix,
              modelToBuildList,
              modelPackage,
              splitName[splitName.length - 1],
              formats,
              useTimeType);
    } else {
      final String simpleType = MapperUtil.getSimpleType(schema, prefix, suffix, useTimeType);
      fieldObject = SchemaFieldObject
                        .builder()
                        .baseName(name)
                        .dataType(simpleType)
                        .dataTypeSimple(simpleType)
                        .restrictions(new SchemaFieldObjectProperties())
                        .constValue(MapperUtil.getConstValue(schema))
                        .build();
    }
    return fieldObject;
  }

  private static String createKey(
      final String modelPackage, final String className, final String separator) {
    return Objects.nonNull(modelPackage)
               ? modelPackage.toUpperCase() + separator + className
               : className;
  }

 
}
