/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import com.sngular.api.generator.test.utils.TestUtils;
import org.apache.commons.collections4.CollectionUtils;

public final class OpenApiGeneratorFixtures {

  final static String GENERATED = "generated";

  final static String TARGET = "target";

  final static List<SpecFile> TEST_ALL_OF = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testAllOf/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testapi")
          .modelPackage("com.sngular.multifileplugin.testapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  final static List<SpecFile> TEST_COMPLEX_ANY_OF = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testComplexAnyOf/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testcomplexanyof.api")
          .modelPackage("com.sngular.multifileplugin.testcomplexanyof.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  final static List<SpecFile> TEST_ALL_OF_LOMBOK = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testAllOf/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.lombok.testapi")
          .modelPackage("com.sngular.multifileplugin.lombok.testapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  final static List<SpecFile> TEST_OVER_WRITE_MODEL_TRUE = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testOverWriteModelTrue/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testoverwriteapi")
          .modelPackage("com.sngular.multifileplugin.testoverwriteapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_API_CLIENT_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiClientGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testapiclient")
          .modelPackage("com.sngular.multifileplugin.testapiclient.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_INLINE_SCHEMA_CREATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testInlineSchemaCreation/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.inlineschemacreation")
          .modelPackage("com.sngular.multifileplugin.inlineschemacreation.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_PARAMETER_WITH_CONTENT_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiParametersWithContentGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.parameterwithcontent")
          .modelPackage("com.sngular.multifileplugin.parameterwithcontent.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_PATH_WITH_SLASH_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiPathWithSpecialCharGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.pathwithspecialchar")
          .modelPackage("com.sngular.multifileplugin.pathwithspecialchar.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_API_REACTIVE_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiReactiveGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.reactivegeneration")
          .modelPackage("com.sngular.multifileplugin.reactivegeneration.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .isReactive(true)
          .build()
  );

  static final List<SpecFile> TEST_API_TAGS_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiTagsGeneration/api-tags-test.yml")
          .apiPackage("com.sngular.multifileplugin.tagsgeneration")
          .modelPackage("com.sngular.multifileplugin.tagsgeneration.model")
          .modelNameSuffix("DTO")
          .useTagsGroup(true)
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_MULTIPLE_REF_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testMultipleRefGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.multipleref")
          .modelPackage("com.sngular.multifileplugin.multipleref.model")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_PATH_PARAMETER_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiPathParameterGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.pathparameter")
          .modelPackage("com.sngular.multifileplugin.pathparameter.model")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_WEB_CLIENT_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testWebClientApiGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.webclientapi")
          .modelPackage("com.sngular.multifileplugin.webclientapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .callMode(true)
          .isReactive(true)
          .build()
  );

  static final List<SpecFile> TEST_CLIENT_PACKAGE_WEB_CLIENT_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testClientPackageWebClientApiGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.clpkgwebclientapi")
          .modelPackage("com.sngular.multifileplugin.clpkgwebclientapi.model")
          .clientPackage("com.sngular.multifileplugin.clpkgwebclientapi.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .callMode(true)
          .build()
  );

  static final List<SpecFile> TEST_REST_CLIENT_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testClientPackageWebClientApiGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.restclient")
          .modelPackage("com.sngular.multifileplugin.restclient.model")
          .clientPackage("com.sngular.multifileplugin.restclient.client")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .callMode(true)
          .build()
  );

  static final List<SpecFile> TEST_ENUMS_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiEnumsGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.enumgeneration")
          .modelPackage("com.sngular.multifileplugin.enumgeneration.model")
          .clientPackage("com.sngular.multifileplugin.enumgeneration.client")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  static final List<SpecFile> TEST_ENUMS_LOMBOK_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiEnumsLombokGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.enumlombokgeneration")
          .modelPackage("com.sngular.multifileplugin.enumlombokgeneration.model")
          .clientPackage("com.sngular.multifileplugin.enumlombokgeneration.client")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_EXTERNAL_REF_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testExternalRefsGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.externalref")
          .modelPackage("com.sngular.multifileplugin.externalref.model")
          .clientPackage("com.sngular.multifileplugin.externalref.client")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  static final List<SpecFile> TEST_ANY_OF_IN_RESPONSE = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testAnyOfInResponse/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testanyofinresponse")
          .modelPackage("com.sngular.multifileplugin.testanyofinresponse.model")
          .clientPackage("com.sngular.multifileplugin.testanyofinresponse.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_ONE_OF_IN_RESPONSE = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testOneOfInResponse/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testoneofinresponse")
          .modelPackage("com.sngular.multifileplugin.testoneofinresponse.model")
          .clientPackage("com.sngular.multifileplugin.testoneofinresponse.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_ADDITIONAL_PROPERTIES = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testAdditionalProperties/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testadditionalproperties")
          .modelPackage("com.sngular.multifileplugin.testadditionalproperties.model")
          .clientPackage("com.sngular.multifileplugin.testadditionalproperties.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_ADDITIONAL_PROPERTIES_WITH_SCHEMA = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testAdditionalPropertiesWithSchema/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testadditionalpropertiesWithSchema")
          .modelPackage("com.sngular.multifileplugin.testadditionalpropertiesWithSchema.model")
          .clientPackage("com.sngular.multifileplugin.testadditionalpropertiesWithSchema.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_ADDITIONAL_PROPERTIES_WITH_UNNAMED_OBJECT = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testAdditionalPropertiesWithUnnamedObject/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject")
          .modelPackage("com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model")
          .clientPackage("com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_COCONUT_SCHEMA = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testCoconutSchema/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testCoconutSchema")
          .modelPackage("com.sngular.multifileplugin.testCoconutSchema.model")
          .clientPackage("com.sngular.multifileplugin.testCoconutSchema.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_CREATE_DTO = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testCreateDTO/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testCreateDto")
          .modelPackage("com.sngular.multifileplugin.testCreateDto.model")
          .clientPackage("com.sngular.multifileplugin.testCreateDto.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static Function<Path, Boolean> validateOneOfInResponse() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testoneofinresponse";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testoneofinresponse/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testoneofinresponse/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testOneOfInResponse/assets/GamesApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testOneOfInResponse/assets/GameDTO.java",
        "openapigenerator/testOneOfInResponse/assets/GameInfoDTO.java",
        "openapigenerator/testOneOfInResponse/assets/InlineResponse200ListGamesOneOfDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testOneOfInResponse/assets/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateAllOf() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testapi";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testapi/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testAllOf/assets/testApi/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testAllOf/assets/testApi/ApiErrorDTO.java",
        "openapigenerator/testAllOf/assets/testApi/ApiTestAllOfDTO.java",
        "openapigenerator/testAllOf/assets/testApi/ApiTestDTO.java",
        "openapigenerator/testAllOf/assets/testApi/ApiTestInfoDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testAllOf/assets/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateComplexAnyOf() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testcomplexanyof/api";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testcomplexanyof/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testcomplexanyof/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testComplexAnyOf/assets/SchemaApi.java",
        "openapigenerator/testComplexAnyOf/assets/SchemaMasterApi.java",
        "openapigenerator/testComplexAnyOf/assets/SchemasApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testComplexAnyOf/assets/ApiArrayFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiBooleanFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiDateFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiDefaultItemDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiEnumFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiMapFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiNumberFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiObjectFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiSchemaDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiSequenceFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiStringFieldDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiTypeArrayDTO.java",
        "openapigenerator/testComplexAnyOf/assets/ApiUnionFieldDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testComplexAnyOf/assets/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateAllOfLombok() {

    final String DEFAULT_LOMBOK_TARGET_API = "generated/com/sngular/multifileplugin/lombok/testapi";

    final String DEFAULT_LOMBOK_MODEL_API = "generated/com/sngular/multifileplugin/lombok/testapi/model";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testAllOf/assets/lombok/TestApi.java");

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testAllOf/assets/lombok/ApiErrorDTO.java",
        "openapigenerator/testAllOf/assets/lombok/ApiTestAllOfDTO.java",
        "openapigenerator/testAllOf/assets/lombok/ApiTestDTO.java",
        "openapigenerator/testAllOf/assets/lombok/ApiTestInfoDTO.java"

    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_LOMBOK_TARGET_API, DEFAULT_LOMBOK_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateOverwriteModeTrue() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testoverwriteapi";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testoverwriteapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testoverwriteapi/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testOverWriteModelTrue/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testOverWriteModelTrue/assets/ApiErrorDTO.java",
        "openapigenerator/testOverWriteModelTrue/assets/ApiTestDTO.java",
        "openapigenerator/testOverWriteModelTrue/assets/ApiTestInfoDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testOverWriteModelTrue/assets/exception/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);

  }

  static Function<Path, Boolean> validateApiClientGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testapiclient";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testapiclient/model";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testApiClientGeneration/assets/TestApi.java");

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testApiClientGeneration/assets/ApiErrorDTO.java",
        "openapigenerator/testApiClientGeneration/assets/ApiTestDTO.java",
        "openapigenerator/testApiClientGeneration/assets/ApiTestInfoDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateInlineSchemaCreation() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/inlineschemacreation";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/inlineschemacreation/model";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testInlineSchemaCreation/assets/RuleApi.java",
        "openapigenerator/testInlineSchemaCreation/assets/RulesApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testInlineSchemaCreation/assets/ApiErrorDTO.java",
        "openapigenerator/testInlineSchemaCreation/assets/ApiTestDTO.java",
        "openapigenerator/testInlineSchemaCreation/assets/ApiTestProcessorDTO.java",
        "openapigenerator/testInlineSchemaCreation/assets/ApiTestTypeDTO.java"

    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateParametersWithSchemaGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/parameterwithcontent";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/parameterwithcontent/model";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testApiParametersWithContentGeneration/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testApiParametersWithContentGeneration/assets/ApiErrorDTO.java",
        "openapigenerator/testApiParametersWithContentGeneration/assets/ApiInlineParameterShowTestByIdTestIdDTO.java",
        "openapigenerator/testApiParametersWithContentGeneration/assets/ApiTestDTO.java",
        "openapigenerator/testApiParametersWithContentGeneration/assets/ApiTestInfoDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validatePathWithSpecialCharGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/pathwithspecialchar";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/pathwithspecialchar/model";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testApiPathWithSpecialCharGeneration/assets/TestApi.java",
        "openapigenerator/testApiPathWithSpecialCharGeneration/assets/TestSchemaApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testApiPathWithSpecialCharGeneration/assets/ApiErrorDTO.java",
        "openapigenerator/testApiPathWithSpecialCharGeneration/assets/ApiTestDTO.java",
        "openapigenerator/testApiPathWithSpecialCharGeneration/assets/ApiTestInfoDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateApiReactiveGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/reactivegeneration";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/reactivegeneration/model";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testApiReactiveGeneration/assets/TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testApiReactiveGeneration/assets/ApiErrorDTO.java",
        "openapigenerator/testApiReactiveGeneration/assets/ApiTestDTO.java",
        "openapigenerator/testApiReactiveGeneration/assets/ApiTestInfoDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateTagsGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/tagsgeneration";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/tagsgeneration/model";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testApiTagsGeneration/assets/TestTagFirstApi.java",
        "openapigenerator/testApiTagsGeneration/assets/TestTagSecondApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testApiTagsGeneration/assets/ErrorDTO.java",
        "openapigenerator/testApiTagsGeneration/assets/TestDTO.java",
        "openapigenerator/testApiTagsGeneration/assets/TestInfoDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateMultipleRefGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/multipleref";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/multipleref/model";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testMultipleRefGeneration/assets/TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testMultipleRefGeneration/assets/InlineResponse200CreateGameDTO.java",
        "openapigenerator/testMultipleRefGeneration/assets/MessageDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validatePathParameterGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/pathparameter";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/pathparameter/model";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testApiPathParameterGeneration/assets/TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testApiPathParameterGeneration/assets/ErrorDTO.java",
        "openapigenerator/testApiPathParameterGeneration/assets/TestDTO.java",
        "openapigenerator/testApiPathParameterGeneration/assets/TestInfoDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateWebClientGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/webclientapi";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/webclientapi/model";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testWebClientApiGeneration/assets/TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testWebClientApiGeneration/assets/ApiErrorDTO.java",
        "openapigenerator/testWebClientApiGeneration/assets/ApiTestDTO.java",
        "openapigenerator/testWebClientApiGeneration/assets/ApiTestInfoDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateClientPackageWebClientGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/clpkgwebclientapi/client";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/clpkgwebclientapi/client/auth";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/clpkgwebclientapi/model/exception";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testClientPackageWebClientApiGeneration/assets/TestClient.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testClientPackageWebClientApiGeneration/assets/TestAuth.java",
        "openapigenerator/testClientPackageWebClientApiGeneration/assets/TestHttpBasicAuth.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testClientPackageWebClientApiGeneration/assets/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);

  }

  static Function<Path, Boolean> validateRestClientGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/restclient";

    final String CLIENT_TARGET_API = "generated/com/sngular/multifileplugin/restclient/client";

    final String CLIENT_MODEL_API = "generated/com/sngular/multifileplugin/restclient/client/auth";

    List<String> expectedTestApiFile = List.of(
        "openapigenerator/testRestClientApiGeneration/assets/TestApi.java"
    );

    List<String> expectedTestClientApiFile = List.of(
        "openapigenerator/testRestClientApiGeneration/assets/client/ApiRestClient.java"
    );

    List<String> expectedTestClientAuthModelFiles = List.of(
        "openapigenerator/testRestClientApiGeneration/assets/client/auth/Authentication.java",
        "openapigenerator/testRestClientApiGeneration/assets/client/auth/HttpBasicAuth.java"
    );

    return (path) ->
               commonTest(path, expectedTestApiFile, Collections.emptyList(), DEFAULT_TARGET_API, null, Collections.emptyList(), null) &&
               commonTest(path, expectedTestClientApiFile, expectedTestClientAuthModelFiles, CLIENT_TARGET_API, CLIENT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateEnumsGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/enumgeneration";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/enumgeneration/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/enumgeneration/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testApiEnumsGeneration/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testApiEnumsGeneration/assets/ApiErrorDTO.java",
        "openapigenerator/testApiEnumsGeneration/assets/ApiTestDTO.java",
        "openapigenerator/testApiEnumsGeneration/assets/ApiTestInfoDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testApiEnumsGeneration/assets/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateEnumsLombokGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/enumlombokgeneration";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/enumlombokgeneration/model";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testApiEnumsLombokGeneration/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testApiEnumsLombokGeneration/assets/ApiErrorDTO.java",
        "openapigenerator/testApiEnumsLombokGeneration/assets/ApiTestDTO.java",
        "openapigenerator/testApiEnumsLombokGeneration/assets/ApiTestInfoDTO.java"

    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateExternalRefGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/externalref";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/externalref/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/externalref/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testExternalRefsGeneration/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testExternalRefsGeneration/assets/ApiTestDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testExternalRefsGeneration/assets/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateAnyOfInResponse() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testanyofinresponse";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testanyofinresponse/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testanyofinresponse/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testAnyOfInResponse/assets/GamesApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testAnyOfInResponse/assets/GameDTO.java",
        "openapigenerator/testAnyOfInResponse/assets/GameInfoDTO.java",
        "openapigenerator/testAnyOfInResponse/assets/InlineResponse200ListGamesAnyOfDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testAnyOfInResponse/assets/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateAdditionalProperties() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testadditionalproperties";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testadditionalproperties/model";

    final List<String> expectedTestApiFile = List.of("openapigenerator/testAdditionalProperties/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of("openapigenerator/testAdditionalProperties/assets/TestDTO.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateAdditionalPropertiesWithSchema() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testadditionalpropertiesWithSchema";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testadditionalpropertiesWithSchema/model";

    final List<String> expectedTestApiFile = List.of("openapigenerator/testAdditionalPropertiesWithSchema/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testAdditionalPropertiesWithSchema/assets/SubtestDTO.java",
        "openapigenerator/testAdditionalPropertiesWithSchema/assets/TestDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateAdditionalPropertiesWithUnnamedObject() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testadditionalpropertiesWithUnnamedObject";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testadditionalpropertiesWithUnnamedObject/model";

    final List<String> expectedTestApiFile = List.of("openapigenerator/testAdditionalPropertiesWithUnnamedObject/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testAdditionalPropertiesWithUnnamedObject/assets/ArraySchemaDTO.java",
        "openapigenerator/testAdditionalPropertiesWithUnnamedObject/assets/EnumSchemaDTO.java",
        "openapigenerator/testAdditionalPropertiesWithUnnamedObject/assets/SecondTestDTO.java",
        "openapigenerator/testAdditionalPropertiesWithUnnamedObject/assets/TestAdditionalPropertyAdditionalPropertyDTO.java",
        "openapigenerator/testAdditionalPropertiesWithUnnamedObject/assets/TestAdditionalPropertyDTO.java",
        "openapigenerator/testAdditionalPropertiesWithUnnamedObject/assets/TestDTO.java",
        "openapigenerator/testAdditionalPropertiesWithUnnamedObject/assets/ThirdTestDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateCoconutSchema() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testCoconutSchema";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testCoconutSchema/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testCoconutSchema/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testCoconutSchema/assets/SchemaApi.java",
        "openapigenerator/testCoconutSchema/assets/SchemaMasterApi.java",
        "openapigenerator/testCoconutSchema/assets/SchemasApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testCoconutSchema/assets/model/ArrayFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/BooleanFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/DateFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/EnumFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/FieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/MapFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/NumberFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/ObjectFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/SchemaDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/SequenceFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/StringFieldDTO.java",
        "openapigenerator/testCoconutSchema/assets/model/UnionFieldDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testCoconutSchema/assets/exception/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateCreateDto() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testCreateDto";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testCreateDto/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testCreateDto/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testCreateDto/assets/TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testCreateDto/assets/model/AddressDTO.java",
      "openapigenerator/testCreateDto/assets/model/PropertiesDTO.java",
      "openapigenerator/testCreateDto/assets/model/TestDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  private static Boolean commonTest(
      final Path resultPath, final List<String> expectedFile, final List<String> expectedModelFiles, final String targetApi, final String targetModel,
      final List<String> expectedExceptionFiles, final String targetException) {
    Boolean result = Boolean.TRUE;
    try {
      Path pathToTarget = Path.of(resultPath.toString(), "target");
      Path pathToTargetApi = pathToTarget.resolve(targetApi);

      File targetApiFolder = pathToTargetApi.toFile();
      assertThat(targetApiFolder).isNotEmptyDirectory();
      TestUtils.validateFiles(expectedFile, targetApiFolder);

      if (!expectedModelFiles.isEmpty()) {
        Path pathToTargetModel = pathToTarget.resolve(targetModel);
        File targetModelFolder = pathToTargetModel.toFile();
        assertThat(targetModelFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedModelFiles, targetModelFolder);
      }

      if (CollectionUtils.isNotEmpty(expectedExceptionFiles)) {
        Path pathToTargetException = pathToTarget.resolve(targetException);
        File targetModelException = pathToTargetException.toFile();
        assertThat(targetModelException).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedExceptionFiles, targetModelException);
      }
    } catch (IOException e) {
      result = Boolean.FALSE;
    }
    return result;
  }

  private OpenApiGeneratorFixtures() {
  }
}
