/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import com.sngular.api.generator.plugin.common.model.TypeConstants.TimeType;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import com.sngular.api.generator.test.utils.TestUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;

@Slf4j
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
          .filePath("openapigenerator/testRestClientApiGeneration/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.restclient")
          .modelPackage("com.sngular.multifileplugin.restclient.model")
          .clientPackage("com.sngular.multifileplugin.restclient.client")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .callMode(true)
          .build()
  );

  static final List<SpecFile> TEST_REST_CLIENT_API_WITH_REQUEST_OBJECTS_GENERATION = List.of(
          SpecFile
                  .builder()
                  .filePath("openapigenerator/testRestClientApiWithRequestObjectGeneration/api-test.yml")
                  .apiPackage("com.sngular.multifileplugin.restclientWithRequestObjects")
                  .modelPackage("com.sngular.multifileplugin.restclientWithRequestObjects.model")
                  .clientPackage("com.sngular.multifileplugin.restclientWithRequestObjects.client")
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

  static final List<SpecFile> TEST_RESTRICTION_SCHEMA = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testRestrictionsSchema/api-rest.yaml")
          .apiPackage("com.sngular.multifileplugin.testRestrictionsSchema")
          .modelPackage("com.sngular.multifileplugin.testRestrictionsSchema.model")
          .clientPackage("com.sngular.multifileplugin.testRestrictionsSchema.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_SIMPLE_BUILD = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testSimpleBuild/api-rest.yaml")
          .apiPackage("com.sngular.multifileplugin.testsimplebuild")
          .modelPackage("com.sngular.multifileplugin.testsimplebuild.model")
          .clientPackage("com.sngular.multifileplugin.testsimplebuild.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  final static List<SpecFile> TEST_VALIDATION_ANNOTATIONS = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testValidationAnnotations/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testapi")
          .modelPackage("com.sngular.multifileplugin.testapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  final static List<SpecFile> TEST_VALIDATION_ANNOTATIONS_LOMBOK = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testValidationAnnotations/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.lombok.testapi")
          .modelPackage("com.sngular.multifileplugin.lombok.testapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_CREATE_DTO = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testCreateDTO/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testCreateDTO")
          .modelPackage("com.sngular.multifileplugin.testCreateDTO.model")
          .clientPackage("com.sngular.multifileplugin.testCreateDTO.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_CREATE_BASIC_DTO = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testCreateBasicDTO/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testCreateBasicDTO")
          .modelPackage("com.sngular.multifileplugin.testCreateBasicDTO.model")
          .clientPackage("com.sngular.multifileplugin.testCreateBasicDTO.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_ISSUE_FAKER = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testIssueFaker/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testissuefaker")
          .modelPackage("com.sngular.multifileplugin.testissuefaker.model")
          .clientPackage("com.sngular.multifileplugin.testissuefaker.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_DATE_TIME = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testDateTime/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testDateTime")
          .modelPackage("com.sngular.multifileplugin.testDateTime.model")
          .clientPackage("com.sngular.multifileplugin.testDateTime.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_DATE_TIME_ZONED = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testDateTimeZoned/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testDateTimeZoned")
          .modelPackage("com.sngular.multifileplugin.testDateTimeZoned.model")
          .clientPackage("com.sngular.multifileplugin.testDateTimeZoned.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .useTimeType(TimeType.ZONED)
          .build()
  );

  static final List<SpecFile> TEST_DATE_TIME_OFFSET = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testDateTimeOffset/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testDateTimeOffset")
          .modelPackage("com.sngular.multifileplugin.testDateTimeOffset.model")
          .clientPackage("com.sngular.multifileplugin.testDateTimeOffset.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .useTimeType(TimeType.OFFSET)
          .build()
  );

  static final List<SpecFile> TEST_LIST_STRING = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testListString/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testListString")
          .modelPackage("com.sngular.multifileplugin.testListString.model")
          .clientPackage("com.sngular.multifileplugin.testListString.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_REFERENCE_FILE = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testReferenceFile/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testReferenceFile")
          .modelPackage("com.sngular.multifileplugin.testReferenceFile.model")
          .clientPackage("com.sngular.multifileplugin.testReferenceFile.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_QUERY_PARAM = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testQueryParam/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testQueryParam")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_API_WITH_NO_COMPONENTS = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testApiWithNoComponents/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testApiWithNoComponents")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_GENERATION_WITH_NO_OPERATION_ID = List.of(
      SpecFile
          .builder()
          .filePath("openapigenerator/testGenerationWithNoOperationId/api-test.yml")
          .apiPackage("com.sngular.multifileplugin.testGenerationWithNoOperationId")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_FORM_DATA_MULTIPART_GENERATION = List.of(
          SpecFile
                  .builder()
                  .filePath("openapigenerator/testFormDataMultipartGeneration/api-test.yml")
                  .apiPackage("com.sngular.multifileplugin.testFormDataMultipartGeneration")
                  .useLombokModelAnnotation(true)
                  .build()
  );

  static Function<Path, Boolean> validateOneOfInResponse() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testoneofinresponse";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testoneofinresponse/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testoneofinresponse/model/exception";

    final String COMMON_PATH = "openapigenerator/testOneOfInResponse/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "GamesApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "GameDTO.java",
        ASSETS_PATH + "GameInfoDTO.java",
        ASSETS_PATH + "InlineResponse200ListGamesOneOfDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateAllOf() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testapi";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testapi/model/exception";

    final String COMMON_PATH = "openapigenerator/testAllOf/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "testApi/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "testApi/ApiErrorDTO.java",
        ASSETS_PATH + "testApi/ApiTestAllOfDTO.java",
        ASSETS_PATH + "testApi/ApiTestDTO.java",
        ASSETS_PATH + "testApi/ApiTestInfoDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateComplexAnyOf() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testcomplexanyof/api";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testcomplexanyof/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testcomplexanyof/model/exception";

    final String COMMON_PATH = "openapigenerator/testComplexAnyOf/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "SchemaApi.java",
        ASSETS_PATH + "SchemaMasterApi.java",
        ASSETS_PATH + "SchemasApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiArrayFieldDTO.java",
        ASSETS_PATH + "ApiBooleanFieldDTO.java",
        ASSETS_PATH + "ApiDateFieldDTO.java",
        ASSETS_PATH + "ApiDefaultItemDTO.java",
        ASSETS_PATH + "ApiEnumFieldDTO.java",
        ASSETS_PATH + "ApiMapFieldDTO.java",
        ASSETS_PATH + "ApiNumberFieldDTO.java",
        ASSETS_PATH + "ApiObjectFieldDTO.java",
        ASSETS_PATH + "ApiSchemaDTO.java",
        ASSETS_PATH + "ApiSequenceFieldDTO.java",
        ASSETS_PATH + "ApiStringFieldDTO.java",
        ASSETS_PATH + "ApiTypeArrayDTO.java",
        ASSETS_PATH + "ApiUnionFieldDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateAllOfLombok() {

    final String DEFAULT_LOMBOK_TARGET_API = "generated/com/sngular/multifileplugin/lombok/testapi";

    final String DEFAULT_LOMBOK_MODEL_API = "generated/com/sngular/multifileplugin/lombok/testapi/model";

    final String COMMON_PATH = "openapigenerator/testAllOf/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "lombok/TestApi.java");

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "lombok/ApiErrorDTO.java",
        ASSETS_PATH + "lombok/ApiTestAllOfDTO.java",
        ASSETS_PATH + "lombok/ApiTestDTO.java",
        ASSETS_PATH + "lombok/ApiTestInfoDTO.java"

    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_LOMBOK_TARGET_API, DEFAULT_LOMBOK_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateOverwriteModeTrue() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testoverwriteapi";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testoverwriteapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testoverwriteapi/model/exception";

    final String COMMON_PATH = "openapigenerator/testOverWriteModelTrue/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestInfoDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "exception/ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);

  }

  static Function<Path, Boolean> validateApiClientGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testapiclient";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testapiclient/model";

    final String COMMON_PATH = "openapigenerator/testApiClientGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java");

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestInfoDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateInlineSchemaCreation() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/inlineschemacreation";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/inlineschemacreation/model";

    final String COMMON_PATH = "openapigenerator/testInlineSchemaCreation/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "RuleApi.java",
        ASSETS_PATH + "RulesApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestProcessorDTO.java",
        ASSETS_PATH + "ApiTestTypeDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateParametersWithSchemaGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/parameterwithcontent";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/parameterwithcontent/model";

    final String COMMON_PATH = "openapigenerator/testApiParametersWithContentGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(ASSETS_PATH + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiInlineParameterTestIdDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestInfoDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validatePathWithSpecialCharGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/pathwithspecialchar";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/pathwithspecialchar/model";

    final String COMMON_PATH = "openapigenerator/testApiPathWithSpecialCharGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java",
        ASSETS_PATH + "TestSchemaApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestInfoDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateApiReactiveGeneration(int springBootVersion) {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/reactivegeneration";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/reactivegeneration/model";

    final String COMMON_PATH = "openapigenerator/testApiReactiveGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + calculateJavaEEPackage(springBootVersion) + "TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestInfoDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateTagsGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/tagsgeneration";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/tagsgeneration/model";

    final String COMMON_PATH = "openapigenerator/testApiTagsGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestTagFirstApi.java",
        ASSETS_PATH + "TestTagSecondApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ErrorDTO.java",
        ASSETS_PATH + "TestDTO.java",
        ASSETS_PATH + "TestInfoDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateMultipleRefGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/multipleref";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/multipleref/model";

    final String COMMON_PATH = "openapigenerator/testMultipleRefGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "InlineResponse200CreateGameDTO.java",
        ASSETS_PATH + "MessageDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validatePathParameterGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/pathparameter";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/pathparameter/model";

    final String COMMON_PATH = "openapigenerator/testApiPathParameterGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ErrorDTO.java",
        ASSETS_PATH + "TestDTO.java",
        ASSETS_PATH + "TestInfoDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateWebClientGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/webclientapi";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/webclientapi/model";

    final String COMMON_PATH = "openapigenerator/testWebClientApiGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestInfoDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> validateClientPackageWebClientGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/clpkgwebclientapi/client";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/clpkgwebclientapi/client/auth";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/clpkgwebclientapi/model/exception";

    final String COMMON_PATH = "openapigenerator/testClientPackageWebClientApiGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestClient.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "TestAuth.java",
        ASSETS_PATH + "TestHttpBasicAuth.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);

  }

  static Function<Path, Boolean> validateRestClientGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/restclient";

    final String CLIENT_TARGET_API = "generated/com/sngular/multifileplugin/restclient/client";

    final String CLIENT_MODEL_API = "generated/com/sngular/multifileplugin/restclient/client/auth";

    final String COMMON_PATH = "openapigenerator/testRestClientApiGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java"
    );

    List<String> expectedTestClientApiFile = List.of(
        ASSETS_PATH + "client/ApiRestClient.java"
    );

    List<String> expectedTestClientAuthModelFiles = List.of(
        ASSETS_PATH + "client/auth/Authentication.java",
        ASSETS_PATH + "client/auth/HttpBasicAuth.java"
    );

    return path ->
               commonTest(path, expectedTestApiFile, Collections.emptyList(), DEFAULT_TARGET_API, null, Collections.emptyList(), null) &&
               commonTest(path, expectedTestClientApiFile, expectedTestClientAuthModelFiles, CLIENT_TARGET_API, CLIENT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateRestClientWithRequestBodyGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/restclientWithRequestObjects";

    final String CLIENT_TARGET_API = "generated/com/sngular/multifileplugin/restclientWithRequestObjects/client";

    final String CLIENT_MODEL_API = "generated/com/sngular/multifileplugin/restclientWithRequestObjects/client/auth";

    final String COMMON_PATH = "openapigenerator/testRestClientApiWithRequestObjectGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
            ASSETS_PATH + "TestApi.java"
    );

    List<String> expectedTestClientApiFile = List.of(
            ASSETS_PATH + "client/ApiRestClient.java"
    );

    List<String> expectedTestClientAuthModelFiles = List.of(
            ASSETS_PATH + "client/auth/Authentication.java",
            ASSETS_PATH + "client/auth/HttpBasicAuth.java"
    );

    return path ->
            commonTest(path, expectedTestApiFile, Collections.emptyList(), DEFAULT_TARGET_API, null, Collections.emptyList(), null) &&
                    commonTest(path, expectedTestClientApiFile, expectedTestClientAuthModelFiles, CLIENT_TARGET_API, CLIENT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateEnumsGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/enumgeneration";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/enumgeneration/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/enumgeneration/model/exception";

    final String COMMON_PATH = "openapigenerator/testApiEnumsGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestInfoDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        "openapigenerator/testApiEnumsGeneration/assets/ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateEnumsLombokGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/enumlombokgeneration";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/enumlombokgeneration/model";

    final String COMMON_PATH = "openapigenerator/testApiEnumsLombokGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiErrorDTO.java",
        ASSETS_PATH + "ApiTestDTO.java",
        ASSETS_PATH + "ApiTestInfoDTO.java"

    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateExternalRefGeneration() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/externalref";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/externalref/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/externalref/model/exception";

    final String COMMON_PATH = "openapigenerator/testExternalRefsGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ApiTestDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateAnyOfInResponse() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testanyofinresponse";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testanyofinresponse/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testanyofinresponse/model/exception";

    final String COMMON_PATH = "openapigenerator/testAnyOfInResponse/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "GamesApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "GameDTO.java",
        ASSETS_PATH + "GameInfoDTO.java",
        ASSETS_PATH + "InlineResponse200ListGamesAnyOfDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateAdditionalProperties() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testadditionalproperties";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testadditionalproperties/model";

    final String COMMON_PATH = "openapigenerator/testAdditionalProperties/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(ASSETS_PATH + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(ASSETS_PATH + "TestDTO.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateAdditionalPropertiesWithSchema() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testadditionalpropertiesWithSchema";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testadditionalpropertiesWithSchema/model";

    final String COMMON_PATH = "openapigenerator/testAdditionalPropertiesWithSchema/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(ASSETS_PATH + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "SubtestDTO.java",
        ASSETS_PATH + "TestDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateAdditionalPropertiesWithUnnamedObject() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testadditionalpropertiesWithUnnamedObject";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testadditionalpropertiesWithUnnamedObject/model";

    final String COMMON_PATH = "openapigenerator/testAdditionalPropertiesWithUnnamedObject/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(ASSETS_PATH + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "ArraySchemaDTO.java",
        ASSETS_PATH + "EnumSchemaDTO.java",
        ASSETS_PATH + "SecondTestDTO.java",
        ASSETS_PATH + "TestDTO.java",
        ASSETS_PATH + "TestValueDTO.java",
        ASSETS_PATH + "TestValueValueDTO.java",
        ASSETS_PATH + "ThirdTestDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateCoconutSchema() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testCoconutSchema";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testCoconutSchema/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testCoconutSchema/model/exception";

    final String COMMON_PATH = "openapigenerator/testCoconutSchema/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "SchemaApi.java",
        ASSETS_PATH + "SchemaMasterApi.java",
        ASSETS_PATH + "SchemasApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "model/ArrayFieldDTO.java",
        ASSETS_PATH + "model/BooleanFieldDTO.java",
        ASSETS_PATH + "model/DateFieldDTO.java",
        ASSETS_PATH + "model/EnumFieldDTO.java",
        ASSETS_PATH + "model/FieldDTO.java",
        ASSETS_PATH + "model/MapFieldDTO.java",
        ASSETS_PATH + "model/NumberFieldDTO.java",
        ASSETS_PATH + "model/ObjectFieldDTO.java",
        ASSETS_PATH + "model/SchemaDTO.java",
        ASSETS_PATH + "model/SequenceFieldDTO.java",
        ASSETS_PATH + "model/StringFieldDTO.java",
        ASSETS_PATH + "model/UnionFieldDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "exception/ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateRestrictionsSchema() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testRestrictionsSchema";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testRestrictionsSchema/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testRestrictionsSchema/model/exception";

    final String COMMON_PATH = "openapigenerator/testRestrictionsSchema/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "SchemaApi.java",
        ASSETS_PATH + "SchemaMasterApi.java",
        ASSETS_PATH + "SchemasApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "model/ArrayFieldDTO.java",
        ASSETS_PATH + "model/BooleanFieldDTO.java",
        ASSETS_PATH + "model/DateFieldDTO.java",
        ASSETS_PATH + "model/EnumFieldDTO.java",
        ASSETS_PATH + "model/FieldDTO.java",
        ASSETS_PATH + "model/FieldValueDTO.java",
        ASSETS_PATH + "model/MapFieldDTO.java",
        ASSETS_PATH + "model/NumberFieldDTO.java",
        ASSETS_PATH + "model/ObjectFieldDTO.java",
        ASSETS_PATH + "model/SchemaDTO.java",
        ASSETS_PATH + "model/SequenceFieldDTO.java",
        ASSETS_PATH + "model/StringFieldDTO.java",
        ASSETS_PATH + "model/UUIDFieldDTO.java",
        ASSETS_PATH + "model/UnionFieldDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "exception/ModelClassException.java");

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateSimpleBuild() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testsimplebuild";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testsimplebuild/model";

    final String COMMON_PATH = "openapigenerator/testSimpleBuild/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "V1Api.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "model/ActivatePolicyDTO.java",
        ASSETS_PATH + "model/AddressDTO.java",
        ASSETS_PATH + "model/CompanyDTO.java",
        ASSETS_PATH + "model/ContactDTO.java",
        ASSETS_PATH + "model/CreditLimitDTO.java",
        ASSETS_PATH + "model/DocumentDTO.java",
        ASSETS_PATH + "model/ErrorResponseDTO.java",
        ASSETS_PATH + "model/InsuredCreditObjectDTO.java",
        ASSETS_PATH + "model/PolicyActivationDTO.java",
        ASSETS_PATH + "model/PolicyClaimDTO.java",
        ASSETS_PATH + "model/PolicySettlementDTO.java",
        ASSETS_PATH + "model/QuoteRequestDTO.java",
        ASSETS_PATH + "model/QuoteResponseDTO.java",
        ASSETS_PATH + "model/QuoteUpdateResponseDTO.java",
        ASSETS_PATH + "model/UpdateQuoteDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateValidationAnnotations(int springBootVersion) {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testapi";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testapi/model/exception";

    final String COMMON_PATH = "openapigenerator/testValidationAnnotations/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/" + calculateJavaEEPackage(springBootVersion);

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "testApi/" + calculateJavaEEPackage(springBootVersion) + "TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "testApi/ApiErrorDTO.java",
        ASSETS_PATH + "testApi/ApiTestAllOfDTO.java",
        ASSETS_PATH + "testApi/ApiTestDTO.java",
        ASSETS_PATH + "testApi/ApiTestInfoDTO.java"

    );

    final List<String> expectedExceptionFiles = List.of(
        ASSETS_PATH + "ModelClassException.java");

    final List<String> expectedValidatorFiles = List.of(
        CUSTOM_VALIDATOR_PATH + "MaxBigDecimal.java",
      CUSTOM_VALIDATOR_PATH + "MaxBigDecimalValidator.java",
      CUSTOM_VALIDATOR_PATH + "MaxDouble.java",
      CUSTOM_VALIDATOR_PATH + "MaxDoubleValidator.java",
      CUSTOM_VALIDATOR_PATH + "MaxFloat.java",
      CUSTOM_VALIDATOR_PATH + "MaxFloatValidator.java",
      CUSTOM_VALIDATOR_PATH + "MaxInteger.java",
      CUSTOM_VALIDATOR_PATH + "MaxIntegerValidator.java",
      CUSTOM_VALIDATOR_PATH + "MaxItems.java",
      CUSTOM_VALIDATOR_PATH + "MaxItemsValidator.java",
        CUSTOM_VALIDATOR_PATH + "MinBigDecimal.java",
      CUSTOM_VALIDATOR_PATH + "MinBigDecimalValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinDouble.java",
      CUSTOM_VALIDATOR_PATH + "MinDoubleValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinFloat.java",
      CUSTOM_VALIDATOR_PATH + "MinFloatValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinInteger.java",
      CUSTOM_VALIDATOR_PATH + "MinIntegerValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinItems.java",
      CUSTOM_VALIDATOR_PATH + "MinItemsValidator.java",
        CUSTOM_VALIDATOR_PATH + "MultipleOf.java",
        CUSTOM_VALIDATOR_PATH + "MultipleOfValidator.java",
        CUSTOM_VALIDATOR_PATH + "NotNull.java",
        CUSTOM_VALIDATOR_PATH + "NotNullValidator.java",
        CUSTOM_VALIDATOR_PATH + "Pattern.java",
        CUSTOM_VALIDATOR_PATH + "PatternValidator.java",
        CUSTOM_VALIDATOR_PATH + "Size.java",
        CUSTOM_VALIDATOR_PATH + "SizeValidator.java",
        CUSTOM_VALIDATOR_PATH + "UniqueItems.java",
        CUSTOM_VALIDATOR_PATH + "UniqueItemsValidator.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API)
                     && customValidatorTest(path, expectedValidatorFiles);
  }

  static Function<Path, Boolean> validateValidationAnnotationsLombok(int springBootTest) {

    final String DEFAULT_LOMBOK_TARGET_API = "generated/com/sngular/multifileplugin/lombok/testapi";

    final String DEFAULT_LOMBOK_MODEL_API = "generated/com/sngular/multifileplugin/lombok/testapi/model";

    final String COMMON_PATH = "openapigenerator/testValidationAnnotations/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "lombok/" + calculateJavaEEPackage(springBootTest) + "TestApi.java");

    List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "lombok/ApiErrorDTO.java",
        ASSETS_PATH + "lombok/ApiTestAllOfDTO.java",
        ASSETS_PATH + "lombok/ApiTestDTO.java",
        ASSETS_PATH + "lombok/ApiTestInfoDTO.java"

    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_LOMBOK_TARGET_API, DEFAULT_LOMBOK_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateCreateDTO() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testCreateDTO";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testCreateDTO/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testCreateDTO/model/exception";

    final String COMMON_PATH = "openapigenerator/testCreateDTO/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "model/AddressDTO.java",
        ASSETS_PATH + "model/PropertiesDTO.java",
        ASSETS_PATH + "model/TestDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateCreateBasicDTO() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testCreateBasicDTO";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testCreateBasicDTO/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testCreateBasicDTO/model/exception";

    final String COMMON_PATH = "openapigenerator/testCreateBasicDTO/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "model/AddressDTO.java",
        ASSETS_PATH + "model/TestDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateIssueFaker() {

    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testissuefaker";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testissuefaker/model";

    final String COMMON_PATH = "openapigenerator/testIssueFaker/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedTestApiFile = List.of(
        ASSETS_PATH + "FakerApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        ASSETS_PATH + "model/ConfigurationDTO.java",
        ASSETS_PATH + "model/FakerFieldDTO.java",
        ASSETS_PATH + "model/FakerSchemaDTO.java",
        ASSETS_PATH + "model/FieldDTO.java",
        ASSETS_PATH + "model/SchemaDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateDateTime() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testDateTime";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testDateTime/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testDateTime/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testDateTime/assets/TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testDateTime/assets/model/TestDateDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateDateTimeZoned() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testDateTimeZoned";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testDateTimeZoned/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testDateTimeZoned/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testDateTimeZoned/assets/TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testDateTimeZoned/assets/model/TestDateDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateDateTimeOffset() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testDateTimeOffset";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testDateTimeOffset/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testDateTimeOffset/model/exception";

    final List<String> expectedTestApiFile = List.of(
        "openapigenerator/testDateTimeOffset/assets/TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testDateTimeOffset/assets/model/TestDateDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateListString() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testListString";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testListString/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testListString/model/exception";

    final List<String> expectedTestApiFiles = List.of(
        "openapigenerator/testListString/assets/TestApi.java",
        "openapigenerator/testListString/assets/TestInlineApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
    );

    return path -> commonTest(path, expectedTestApiFiles, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateReferenceFile() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testReferenceFile";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testReferenceFile/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testReferenceFile/model/exception";

    final List<String> expectedTestApiFiles = List.of(
        "openapigenerator/testReferenceFile/assets/TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
        "openapigenerator/testReferenceFile/assets/TestDTO.java"
    );

    return path -> commonTest(path, expectedTestApiFiles, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateQueryParam() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testQueryParam";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testQueryParam/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testQueryParam/model/exception";

    final List<String> expectedTestApiFiles = List.of(
        "openapigenerator/testQueryParam/assets/TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
    );

    return path -> commonTest(path, expectedTestApiFiles, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateApiWithNoComponents() {
    final String DEFAULT_TARGET_API = "generated/com/sngular/multifileplugin/testApiWithNoComponents";

    final String DEFAULT_MODEL_API = "generated/com/sngular/multifileplugin/testApiWithNoComponents/model";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/multifileplugin/testApiWithNoComponents/model/exception";

    final List<String> expectedTestApiFiles = List.of(
        "openapigenerator/testApiWithNoComponents/assets/TestApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
    );

    return path -> commonTest(path, expectedTestApiFiles, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), DEFAULT_EXCEPTION_API);

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
    } catch (URISyntaxException | NullPointerException e) {
      result = Boolean.FALSE;
      log.error(e.getLocalizedMessage());
    }
    return result;
  }

  private static Boolean customValidatorTest(final Path resultPath, final List<String> expectedValidatorFiles) {
    Boolean result = Boolean.TRUE;
    try {
      final Path pathToTarget = Path.of(resultPath.toString(), "target");

      if (!expectedValidatorFiles.isEmpty()) {
        final Path pathToTargetCustomValidator = pathToTarget.resolve("generated/com/sngular/multifileplugin/testapi/model/customvalidator");
        final File targetCustomValidatorFolder = pathToTargetCustomValidator.toFile();
        assertThat(targetCustomValidatorFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedValidatorFiles, targetCustomValidatorFolder);
      }
    } catch (URISyntaxException | NullPointerException e) {
      result = Boolean.FALSE;
      log.error(e.getLocalizedMessage());
    }
    return result;
  }

  private OpenApiGeneratorFixtures() {
  }

  private static String calculateJavaEEPackage(int springBootVersion) {
    if (3 <= springBootVersion) {
      return "jakarta/";
    } else {
      return "javax/";
    }
  }
}
