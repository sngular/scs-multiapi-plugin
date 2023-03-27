/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class OpenApiGeneratorTest {

  @TempDir
  static Path baseDir;

  private static OpenApiGenerator openApiGenerator;

  @BeforeAll
  static void setup() {
    openApiGenerator =
        new OpenApiGenerator(Boolean.TRUE, OpenApiGeneratorFixtures.GENERATED, "groupId", new File(baseDir.toAbsolutePath() + File.separator + OpenApiGeneratorFixtures.TARGET),
                             baseDir.toFile());
  }

  static Stream<Arguments> fileSpecToProcess() {
    return Stream.of(
        Arguments.of("testAllOf", OpenApiGeneratorFixtures.TEST_ALL_OF, OpenApiGeneratorFixtures.validateAllOf()),
        Arguments.of("testComplexAnyOf", OpenApiGeneratorFixtures.TEST_COMPLEX_ANY_OF, OpenApiGeneratorFixtures.validateComplexAnyOf()),
        Arguments.of("testAllOfLombok", OpenApiGeneratorFixtures.TEST_ALL_OF_LOMBOK, OpenApiGeneratorFixtures.validateAllOfLombok()),
        Arguments.of("testOverWriteModelTrue", OpenApiGeneratorFixtures.TEST_OVER_WRITE_MODEL_TRUE, OpenApiGeneratorFixtures.validateOverwriteModeTrue()),
        Arguments.of("testApiClientGeneration", OpenApiGeneratorFixtures.TEST_API_CLIENT_GENERATION, OpenApiGeneratorFixtures.validateApiClientGeneration()),
        Arguments.of("testInlineSchemaCreation", OpenApiGeneratorFixtures.TEST_INLINE_SCHEMA_CREATION, OpenApiGeneratorFixtures.validateInlineSchemaCreation()),
        Arguments.of("testApiParametersWithContentGeneration", OpenApiGeneratorFixtures.TEST_PARAMETER_WITH_CONTENT_GENERATION,
                     OpenApiGeneratorFixtures.validateParametersWithSchemaGeneration()),
        Arguments.of("testApiPathWithSpecialCharGeneration", OpenApiGeneratorFixtures.TEST_PATH_WITH_SLASH_GENERATION,
                     OpenApiGeneratorFixtures.validatePathWithSpecialCharGeneration()),
        Arguments.of("testApiReactiveGeneration", OpenApiGeneratorFixtures.TEST_API_REACTIVE_GENERATION,
                     OpenApiGeneratorFixtures.validateApiReactiveGeneration()),
        Arguments.of("testApiTagsGeneration", OpenApiGeneratorFixtures.TEST_API_TAGS_GENERATION,
                     OpenApiGeneratorFixtures.validateTagsGeneration()),
        Arguments.of("testMultipleRefGeneration", OpenApiGeneratorFixtures.TEST_MULTIPLE_REF_GENERATION,
                     OpenApiGeneratorFixtures.validateMultipleRefGeneration()),
        Arguments.of("testApiPathParameterGeneration", OpenApiGeneratorFixtures.TEST_PATH_PARAMETER_GENERATION,
                     OpenApiGeneratorFixtures.validatePathParameterGeneration()),
        Arguments.of("testWebClientApiGeneration", OpenApiGeneratorFixtures.TEST_WEB_CLIENT_GENERATION,
                     OpenApiGeneratorFixtures.validateWebClientGeneration()),
        Arguments.of("testClientPackageWebClientApiGeneration", OpenApiGeneratorFixtures.TEST_CLIENT_PACKAGE_WEB_CLIENT_GENERATION,
                     OpenApiGeneratorFixtures.validateClientPackageWebClientGeneration()),
        Arguments.of("testRestClientApiGeneration", OpenApiGeneratorFixtures.TEST_REST_CLIENT_GENERATION,
                     OpenApiGeneratorFixtures.validateRestClientGeneration()),
        Arguments.of("testApiEnumsGeneration", OpenApiGeneratorFixtures.TEST_ENUMS_GENERATION,
                     OpenApiGeneratorFixtures.validateEnumsGeneration()),
        Arguments.of("testApiEnumsLombokGeneration", OpenApiGeneratorFixtures.TEST_ENUMS_LOMBOK_GENERATION,
                     OpenApiGeneratorFixtures.validateEnumsLombokGeneration()),
        Arguments.of("testExternalRefsGeneration", OpenApiGeneratorFixtures.TEST_EXTERNAL_REF_GENERATION,
                     OpenApiGeneratorFixtures.validateExternalRefGeneration()),
        Arguments.of("testAnyOfInResponse", OpenApiGeneratorFixtures.TEST_ANY_OF_IN_RESPONSE,
                     OpenApiGeneratorFixtures.validateAnyOfInResponse()),
        Arguments.of("testOneOfInResponse", OpenApiGeneratorFixtures.TEST_ONE_OF_IN_RESPONSE,
                     OpenApiGeneratorFixtures.validateOneOfInResponse()),
        Arguments.of("testAdditionalProperties", OpenApiGeneratorFixtures.TEST_ADDITIONAL_PROPERTIES,
                     OpenApiGeneratorFixtures.validateAdditionalProperties()),
        Arguments.of("testAdditionalPropertiesWithSchema", OpenApiGeneratorFixtures.TEST_ADDITIONAL_PROPERTIES_WITH_SCHEMA,
                     OpenApiGeneratorFixtures.validateAdditionalPropertiesWithSchema()),
        Arguments.of("testAdditionalPropertiesWithUnnamedObject", OpenApiGeneratorFixtures.TEST_ADDITIONAL_PROPERTIES_WITH_UNNAMED_OBJECT,
                     OpenApiGeneratorFixtures.validateAdditionalPropertiesWithUnnamedObject()),
        Arguments.of("testCoconutSchema", OpenApiGeneratorFixtures.TEST_COCONUT_SCHEMA,
                     OpenApiGeneratorFixtures.validateCoconutSchema()),
        Arguments.of("testValidationAnnotations", OpenApiGeneratorFixtures.TEST_VALIDATION_ANNOTATIONS,
                     OpenApiGeneratorFixtures.validateValidationAnnotations()),
        Arguments.of("testValidationAnnotationsLombok", OpenApiGeneratorFixtures.TEST_VALIDATION_ANNOTATIONS_LOMBOK,
                     OpenApiGeneratorFixtures.validateValidationAnnotationsLombok()),
        Arguments.of("testCreateDTO", OpenApiGeneratorFixtures.TEST_CREATE_DTO,
                     OpenApiGeneratorFixtures.validateCreateDTO())
    );
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpec(final String type, final List<SpecFile> specFileList, final Function<Path, Boolean> validation) {
    openApiGenerator.processFileSpec(specFileList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }
}