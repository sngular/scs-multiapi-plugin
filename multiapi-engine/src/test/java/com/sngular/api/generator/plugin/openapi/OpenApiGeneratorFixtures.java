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

import com.sngular.api.generator.test.utils.TestUtils;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.commons.collections4.CollectionUtils;

public final class OpenApiGeneratorFixtures {

  final static String GENERATED = "generated";

  final static String TARGET = "target";

  final static List<SpecFile> TEST_ALL_OF = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testAllOf/api-test.yml")
          .apiPackage("net.coru.multifileplugin.testapi")
          .modelPackage("net.coru.multifileplugin.testapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  final static List<SpecFile> TEST_COMPLEX_ANY_OF = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testComplexAnyOf/api-test.yml")
          .apiPackage("net.coru.multifileplugin.testcomplexanyof.api")
          .modelPackage("net.coru.multifileplugin.testcomplexanyof.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  final static List<SpecFile> TEST_ALL_OF_LOMBOK = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testAllOf/api-test.yml")
          .apiPackage("net.coru.multifileplugin.lombok.testapi")
          .modelPackage("net.coru.multifileplugin.lombok.testapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  final static List<SpecFile> TEST_OVER_WRITE_MODEL_TRUE = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testOverWriteModelTrue/api-test.yml")
          .apiPackage("net.coru.multifileplugin.testoverwriteapi")
          .modelPackage("net.coru.multifileplugin.testoverwriteapi.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_API_CLIENT_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testApiClientGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.testapiclient")
          .modelPackage("net.coru.multifileplugin.testapiclient.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_INLINE_SCHEMA_CREATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testInlineSchemaCreation/api-test.yml")
          .apiPackage("net.coru.multifileplugin.inlineschemacreation")
          .modelPackage("net.coru.multifileplugin.inlineschemacreation.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_PARAMETER_WITH_CONTENT_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testApiParametersWithContentGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.parameterwithcontent")
          .modelPackage("net.coru.multifileplugin.parameterwithcontent.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_PATH_WITH_SLASH_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testApiPathWithSpecialCharGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.pathwithspecialchar")
          .modelPackage("net.coru.multifileplugin.pathwithspecialchar.model")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_API_REACTIVE_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testApiReactiveGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.reactivegeneration")
          .modelPackage("net.coru.multifileplugin.reactivegeneration.model")
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
          .apiPackage("net.coru.multifileplugin.tagsgeneration")
          .modelPackage("net.coru.multifileplugin.tagsgeneration.model")
          .modelNameSuffix("DTO")
          .useTagsGroup(true)
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_MULTIPLE_REF_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testMultipleRefGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.multipleref")
          .modelPackage("net.coru.multifileplugin.multipleref.model")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_PATH_PARAMETER_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testApiPathParameterGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.pathparameter")
          .modelPackage("net.coru.multifileplugin.pathparameter.model")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .build()
  );

  static final List<SpecFile> TEST_WEB_CLIENT_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testWebClientApiGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.webclientapi")
          .modelPackage("net.coru.multifileplugin.webclientapi.model")
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
          .apiPackage("net.coru.multifileplugin.clpkgwebclientapi")
          .modelPackage("net.coru.multifileplugin.clpkgwebclientapi.model")
          .clientPackage("net.coru.multifileplugin.clpkgwebclientapi.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(false)
          .callMode(true)
          .build()
  );

  static final List<SpecFile> TEST_REST_CLIENT_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testClientPackageWebClientApiGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.restclient")
          .modelPackage("net.coru.multifileplugin.restclient.model")
          .clientPackage("net.coru.multifileplugin.restclient.client")
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
          .apiPackage("net.coru.multifileplugin.enumgeneration")
          .modelPackage("net.coru.multifileplugin.enumgeneration.model")
          .clientPackage("net.coru.multifileplugin.enumgeneration.client")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  static final List<SpecFile> TEST_ENUMS_LOMBOK_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testApiEnumsLombokGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.enumlombokgeneration")
          .modelPackage("net.coru.multifileplugin.enumlombokgeneration.model")
          .clientPackage("net.coru.multifileplugin.enumlombokgeneration.client")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_EXTERNAL_REF_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testExternalRefsGeneration/api-test.yml")
          .apiPackage("net.coru.multifileplugin.externalref")
          .modelPackage("net.coru.multifileplugin.externalref.model")
          .clientPackage("net.coru.multifileplugin.externalref.client")
          .modelNamePrefix("Api")
          .modelNameSuffix("DTO")
          .build()
  );

  static final List<SpecFile> TEST_ANY_OF_IN_RESPONSE = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testAnyOfInResponse/api-test.yml")
          .apiPackage("net.coru.multifileplugin.testanyofinresponse")
          .modelPackage("net.coru.multifileplugin.testanyofinresponse.model")
          .clientPackage("net.coru.multifileplugin.testanyofinresponse.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static final List<SpecFile> TEST_ONE_OF_IN_RESPONSE = List.of(
    SpecFile
          .builder()
          .filePath("openapigenerator/testOneOfInResponse/api-test.yml")
          .apiPackage("net.coru.multifileplugin.testoneofinresponse")
          .modelPackage("net.coru.multifileplugin.testoneofinresponse.model")
          .clientPackage("net.coru.multifileplugin.testoneofinresponse.client")
          .modelNameSuffix("DTO")
          .useLombokModelAnnotation(true)
          .build()
  );

  static Function<Path, Boolean> validateOneOfInResponse() {

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testoneofinresponse";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testoneofinresponse/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/testoneofinresponse/model/exception";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testapi";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/testapi/model/exception";

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

  static Function<Path, Boolean> validateComplesAnyOf() {
    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testcomplexanyof/api";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testcomplexanyof/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/testcomplexanyof/model/exception";

    final List<String> expectedTestApiFile = List.of(
      "openapigenerator/testComplexAnyOf/assets/SchemaApi.java",
      "openapigenerator/testComplexAnyOf/assets/SchemaMasterApi.java",
      "openapigenerator/testComplexAnyOf/assets/SchemasApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testComplexAnyOf/assets/ApiArrayFieldDTO.java",
      "openapigenerator/testComplexAnyOf/assets/ApiBooleanFieldDTO.java",
      "openapigenerator/testComplexAnyOf/assets/ApiDateFieldDTO.java",
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

    final String DEFAULT_LOMBOK_TARGET_API = "generated/net/coru/multifileplugin/lombok/testapi";

    final String DEFAULT_LOMBOK_MODEL_API = "generated/net/coru/multifileplugin/lombok/testapi/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testoverwriteapi";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testoverwriteapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/testoverwriteapi/model/exception";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testapiclient";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testapiclient/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/inlineschemacreation";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/inlineschemacreation/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/parameterwithcontent";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/parameterwithcontent/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/pathwithspecialchar";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/pathwithspecialchar/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/reactivegeneration";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/reactivegeneration/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/tagsgeneration";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/tagsgeneration/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/multipleref";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/multipleref/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/pathparameter";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/pathparameter/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/webclientapi";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/webclientapi/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/clpkgwebclientapi/client";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/clpkgwebclientapi/client/auth";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/clpkgwebclientapi/model/exception";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/restclient";

    final String CLIENT_TARGET_API = "generated/net/coru/multifileplugin/restclient/client";

    final String CLIENT_MODEL_API = "generated/net/coru/multifileplugin/restclient/client/auth";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/enumgeneration";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/enumgeneration/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/enumgeneration/model/exception";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/enumlombokgeneration";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/enumlombokgeneration/model";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/externalref";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/externalref/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/externalref/model/exception";

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

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testanyofinresponse";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testanyofinresponse/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/testanyofinresponse/model/exception";

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
