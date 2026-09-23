/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.function.Function;
import java.util.stream.Stream;

import com.sngular.api.generator.plugin.exception.InvalidAPIException;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.CleanupMode;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class OpenApiGeneratorTest {

  private static final int SPRING_BOOT_VERSION = 2;
  @TempDir(cleanup = CleanupMode.NEVER)
  static Path baseDir;
  private static OpenApiGenerator openApiGenerator;

  @BeforeAll
  static void setup() {
    openApiGenerator =
        new OpenApiGenerator(SPRING_BOOT_VERSION,
            Boolean.TRUE,
            new File(baseDir.toAbsolutePath() + File.separator + OpenApiGeneratorFixtures.TARGET),
            OpenApiGeneratorFixtures.GENERATED,
            "groupId",
            Path.of("src", "test", "resources").toFile());
  }

  static Stream<Arguments> fileSpecToProcess() {
    return Stream.of(
        Arguments.of("testAllOf", OpenApiGeneratorFixtures.TEST_ALL_OF,
            OpenApiGeneratorFixtures.validateAllOf()),
        Arguments.of("testReservedWordsGeneration", OpenApiGeneratorFixtures.TEST_RESERVED_WORDS_GENERATION,
            OpenApiGeneratorFixtures.validateReservedWordsGeneration()),
        Arguments.of("testParameterWithSchemaRefGeneration", OpenApiGeneratorFixtures.TEST_PARAMETER_WITH_SCHEMA_REF_GENERATION,
            OpenApiGeneratorFixtures.validateParameterWithSchemaRefGeneration()),
        Arguments.of("testParameterWithSchemaRefClientGeneration", OpenApiGeneratorFixtures.TEST_PARAMETER_WITH_SCHEMA_REF_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateParameterWithSchemaRefClientGeneration()),
        Arguments.of("testRareCharsNamesGeneration", OpenApiGeneratorFixtures.TEST_RARE_CHARS_NAMES_GENERATION,
            OpenApiGeneratorFixtures.validateRareCharsNamesGeneration()),
        Arguments.of("testRareCharsNamesRestClientGeneration", OpenApiGeneratorFixtures.TEST_RARE_CHARS_NAMES_REST_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateRareCharsNamesRestClientGeneration()),
        Arguments.of("testRareCharsNamesWebClientGeneration", OpenApiGeneratorFixtures.TEST_RARE_CHARS_NAMES_WEB_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateRareCharsNamesWebClientGeneration()),
        Arguments.of("testComplexAnyOf", OpenApiGeneratorFixtures.TEST_COMPLEX_ANY_OF,
            OpenApiGeneratorFixtures.validateComplexAnyOf()),
        Arguments.of("testAllOfLombok", OpenApiGeneratorFixtures.TEST_ALL_OF_LOMBOK,
            OpenApiGeneratorFixtures.validateAllOfLombok()),
        Arguments.of("testOverWriteModelTrue", OpenApiGeneratorFixtures.TEST_OVER_WRITE_MODEL_TRUE,
            OpenApiGeneratorFixtures.validateOverwriteModeTrue()),
        Arguments.of("testApiClientGeneration", OpenApiGeneratorFixtures.TEST_API_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateApiClientGeneration()),
        Arguments.of("testInlineSchemaCreation", OpenApiGeneratorFixtures.TEST_INLINE_SCHEMA_CREATION,
            OpenApiGeneratorFixtures.validateInlineSchemaCreation()),
        Arguments.of("testApiParametersWithContentGeneration", OpenApiGeneratorFixtures.TEST_PARAMETER_WITH_CONTENT_GENERATION,
            OpenApiGeneratorFixtures.validateParametersWithSchemaGeneration()),
        Arguments.of("testApiPathWithSpecialCharGeneration", OpenApiGeneratorFixtures.TEST_PATH_WITH_SLASH_GENERATION,
            OpenApiGeneratorFixtures.validatePathWithSpecialCharGeneration()),
        Arguments.of("testApiReactiveGeneration", OpenApiGeneratorFixtures.TEST_API_REACTIVE_GENERATION,
            OpenApiGeneratorFixtures.validateApiReactiveGeneration(SPRING_BOOT_VERSION)),
        Arguments.of("testReactiveFormDataMultipart", OpenApiGeneratorFixtures.TEST_REACTIVE_FORM_DATA_MULTIPART,
            OpenApiGeneratorFixtures.validateReactiveFormDataMultipart()),
        Arguments.of("testBinaryBodyResource", OpenApiGeneratorFixtures.TEST_BINARY_BODY_RESOURCE,
            OpenApiGeneratorFixtures.validateBinaryBodyResource()),
        Arguments.of("testModelPackageDefault", OpenApiGeneratorFixtures.TEST_MODEL_PACKAGE_DEFAULT,
            OpenApiGeneratorFixtures.validateModelPackageDefault()),
        Arguments.of("testReactiveBinaryBodyResource", OpenApiGeneratorFixtures.TEST_REACTIVE_BINARY_BODY_RESOURCE,
            OpenApiGeneratorFixtures.validateReactiveBinaryBodyResource()),
        Arguments.of("testApiTagsGeneration", OpenApiGeneratorFixtures.TEST_API_TAGS_GENERATION,
            OpenApiGeneratorFixtures.validateTagsGeneration()),
        Arguments.of("testMultipleRefGeneration", OpenApiGeneratorFixtures.TEST_MULTIPLE_REF_GENERATION,
            OpenApiGeneratorFixtures.validateMultipleRefGeneration()),
        Arguments.of("testApiPathParameterGeneration", OpenApiGeneratorFixtures.TEST_PATH_PARAMETER_GENERATION,
            OpenApiGeneratorFixtures.validatePathParameterGeneration()),
        Arguments.of("testPathLevelInlineParameterGeneration", OpenApiGeneratorFixtures.TEST_PATH_LEVEL_INLINE_PARAMETER_GENERATION,
            OpenApiGeneratorFixtures.validatePathLevelInlineParameterGeneration()),
        Arguments.of("testNoDescriptionRefParameterGeneration", OpenApiGeneratorFixtures.TEST_NO_DESCRIPTION_REF_PARAMETER,
            OpenApiGeneratorFixtures.validateNoDescriptionRefParameterGeneration()),
        Arguments.of("testWebClientApiGeneration", OpenApiGeneratorFixtures.TEST_WEB_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateWebClientGeneration()),
        Arguments.of("testClientPackageWebClientApiGeneration", OpenApiGeneratorFixtures.TEST_CLIENT_PACKAGE_WEB_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateClientPackageWebClientGeneration()),
        Arguments.of("testClientPackageWebClientBearerAuthGeneration", OpenApiGeneratorFixtures.TEST_CLIENT_PACKAGE_WEB_CLIENT_BEARER_AUTH_GENERATION,
            OpenApiGeneratorFixtures.validateClientPackageWebClientBearerAuthGeneration()),
        Arguments.of("testRestClientApiGeneration", OpenApiGeneratorFixtures.TEST_REST_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateRestClientGeneration()),
        Arguments.of("testRestClientApiWithRequestObjectGeneration", OpenApiGeneratorFixtures.TEST_REST_CLIENT_API_WITH_REQUEST_OBJECTS_GENERATION,
            OpenApiGeneratorFixtures.validateRestClientWithRequestBodyGeneration()),
        Arguments.of("testApiEnumsGeneration", OpenApiGeneratorFixtures.TEST_ENUMS_GENERATION,
            OpenApiGeneratorFixtures.validateEnumsGeneration()),
        Arguments.of("testApiEnumsLombokGeneration", OpenApiGeneratorFixtures.TEST_ENUMS_LOMBOK_GENERATION,
            OpenApiGeneratorFixtures.validateEnumsLombokGeneration()),
        Arguments.of("testExternalRefsGeneration", OpenApiGeneratorFixtures.TEST_EXTERNAL_REF_GENERATION,
            OpenApiGeneratorFixtures.validateExternalRefGeneration()),
        Arguments.of("testOpenApi31Types", OpenApiGeneratorFixtures.TEST_OPEN_API_31_TYPES,
            OpenApiGeneratorFixtures.validateOpenApi31Types()),
        Arguments.of("testRefWithDescription", OpenApiGeneratorFixtures.TEST_REF_WITH_DESCRIPTION,
            OpenApiGeneratorFixtures.validateRefWithDescription()),
        Arguments.of("testOpenApi31Completeness", OpenApiGeneratorFixtures.TEST_OPEN_API_31_COMPLETENESS,
            OpenApiGeneratorFixtures.validateOpenApi31Completeness()),
        Arguments.of("testWebhooks", OpenApiGeneratorFixtures.TEST_WEBHOOKS,
            OpenApiGeneratorFixtures.validateWebhooks()),
        Arguments.of("testOpenApi31Union", OpenApiGeneratorFixtures.TEST_OPEN_API_31_UNION,
            OpenApiGeneratorFixtures.validateOpenApi31Union()),
        Arguments.of("testWebhookPathCollision", OpenApiGeneratorFixtures.TEST_WEBHOOK_PATH_COLLISION,
            OpenApiGeneratorFixtures.validateWebhookPathCollision()),
        Arguments.of("testExternalPathItemRefsGeneration", OpenApiGeneratorFixtures.TEST_EXTERNAL_PATH_ITEM_REF_GENERATION,
            OpenApiGeneratorFixtures.validateExternalPathItemRefGeneration()),
        Arguments.of("testExternalFragmentPathRefsGeneration", OpenApiGeneratorFixtures.TEST_EXTERNAL_FRAGMENT_PATH_REF_GENERATION,
            OpenApiGeneratorFixtures.validateExternalFragmentPathRefGeneration()),
        Arguments.of("testExternalComponentSchemaRefs", OpenApiGeneratorFixtures.TEST_EXTERNAL_COMPONENT_SCHEMA_REFS,
            OpenApiGeneratorFixtures.validateExternalComponentSchemaRefs()),
        Arguments.of("testInlineSchemaNameClashes", OpenApiGeneratorFixtures.TEST_INLINE_SCHEMA_NAME_CLASHES,
            OpenApiGeneratorFixtures.validateInlineSchemaNameClashes()),
        Arguments.of("testParameterBinding", OpenApiGeneratorFixtures.TEST_PARAMETER_BINDING,
            OpenApiGeneratorFixtures.validateParameterBinding("parameterbinding", "imperative")),
        Arguments.of("testParameterBindingReactive", OpenApiGeneratorFixtures.TEST_PARAMETER_BINDING_REACTIVE,
            OpenApiGeneratorFixtures.validateParameterBinding("parameterbindingreactive", "reactive")),
        Arguments.of("testNestedExternalRefs", OpenApiGeneratorFixtures.TEST_NESTED_EXTERNAL_REFS,
            OpenApiGeneratorFixtures.validateNestedExternalRefs()),
        Arguments.of("testNestedRefInAllOf", OpenApiGeneratorFixtures.TEST_NESTED_REF_IN_ALLOF,
            OpenApiGeneratorFixtures.validateNestedRefInAllOf()),
        Arguments.of("testRefToAllOfProperty", OpenApiGeneratorFixtures.TEST_REF_TO_ALL_OF_PROPERTY,
            OpenApiGeneratorFixtures.validateRefToAllOfProperty()),
        Arguments.of("testNoContentResponses", OpenApiGeneratorFixtures.TEST_NO_CONTENT_RESPONSES,
            OpenApiGeneratorFixtures.validateNoContentResponses()),
        Arguments.of("testExternalResponseRef", OpenApiGeneratorFixtures.TEST_EXTERNAL_RESPONSE_REF,
            OpenApiGeneratorFixtures.validateExternalResponseRef()),
        Arguments.of("testExternalSchemaFileRef", OpenApiGeneratorFixtures.TEST_EXTERNAL_SCHEMA_FILE_REF,
            OpenApiGeneratorFixtures.validateExternalSchemaFileRef()),
        Arguments.of("testExternalResponseRef", OpenApiGeneratorFixtures.TEST_EXTERNAL_RESPONSE_REF,
            OpenApiGeneratorFixtures.validateExternalResponseRef()),
        Arguments.of("testAnyOfInResponse", OpenApiGeneratorFixtures.TEST_ANY_OF_IN_RESPONSE,
            OpenApiGeneratorFixtures.validateAnyOfInResponse()),
        Arguments.of("testOneOfInResponse", OpenApiGeneratorFixtures.TEST_ONE_OF_IN_RESPONSE,
            OpenApiGeneratorFixtures.validateOneOfInResponse()),
        Arguments.of("testAdditionalProperties", OpenApiGeneratorFixtures.TEST_ADDITIONAL_PROPERTIES,
            OpenApiGeneratorFixtures.validateAdditionalProperties()),
        Arguments.of("testAdditionalPropertiesFalse", OpenApiGeneratorFixtures.TEST_ADDITIONAL_PROPERTIES_FALSE,
            OpenApiGeneratorFixtures.validateAdditionalPropertiesFalse()),
        Arguments.of("testAdditionalPropertiesWithSchema", OpenApiGeneratorFixtures.TEST_ADDITIONAL_PROPERTIES_WITH_SCHEMA,
            OpenApiGeneratorFixtures.validateAdditionalPropertiesWithSchema()),
        Arguments.of("testAdditionalPropertiesWithUnnamedObject", OpenApiGeneratorFixtures.TEST_ADDITIONAL_PROPERTIES_WITH_UNNAMED_OBJECT,
            OpenApiGeneratorFixtures.validateAdditionalPropertiesWithUnnamedObject()),
        Arguments.of("testCoconutSchema", OpenApiGeneratorFixtures.TEST_COCONUT_SCHEMA,
            OpenApiGeneratorFixtures.validateCoconutSchema()),
        Arguments.of("testValidationAnnotations", OpenApiGeneratorFixtures.TEST_VALIDATION_ANNOTATIONS,
            OpenApiGeneratorFixtures.validateValidationAnnotations(SPRING_BOOT_VERSION)),
        Arguments.of("testValidationAnnotationsLombok", OpenApiGeneratorFixtures.TEST_VALIDATION_ANNOTATIONS_LOMBOK,
            OpenApiGeneratorFixtures.validateValidationAnnotationsLombok(SPRING_BOOT_VERSION)),
        Arguments.of("testCreateDTO", OpenApiGeneratorFixtures.TEST_CREATE_DTO,
            OpenApiGeneratorFixtures.validateCreateDTO()),
        Arguments.of("testCreateDTOWithEnum", OpenApiGeneratorFixtures.TEST_CREATE_DTO_WITH_ENUM,
            OpenApiGeneratorFixtures.validateCreateDTOWithEnum()),
        Arguments.of("testCreateBasicDTO", OpenApiGeneratorFixtures.TEST_CREATE_BASIC_DTO,
            OpenApiGeneratorFixtures.validateCreateBasicDTO()),
        Arguments.of("testIssueFaker", OpenApiGeneratorFixtures.TEST_ISSUE_FAKER,
            OpenApiGeneratorFixtures.validateIssueFaker()),
        Arguments.of("testDateTime", OpenApiGeneratorFixtures.TEST_DATE_TIME,
            OpenApiGeneratorFixtures.validateDateTime()),
        Arguments.of("testDateTimeZoned", OpenApiGeneratorFixtures.TEST_DATE_TIME_ZONED,
            OpenApiGeneratorFixtures.validateDateTimeZoned()),
        Arguments.of("testDateTimeOffset", OpenApiGeneratorFixtures.TEST_DATE_TIME_OFFSET,
            OpenApiGeneratorFixtures.validateDateTimeOffset()),
        Arguments.of("testListString", OpenApiGeneratorFixtures.TEST_LIST_STRING,
            OpenApiGeneratorFixtures.validateListString()),
        Arguments.of("testReferenceFile", OpenApiGeneratorFixtures.TEST_REFERENCE_FILE,
            OpenApiGeneratorFixtures.validateReferenceFile()),
        Arguments.of("testReferenceFileNoComponents", OpenApiGeneratorFixtures.TEST_REFERENCE_FILE_NO_COMPONENTS,
            OpenApiGeneratorFixtures.validateReferenceFileNoComponents()),
        Arguments.of("testQueryParam", OpenApiGeneratorFixtures.TEST_QUERY_PARAM,
            OpenApiGeneratorFixtures.validateQueryParam()),
        Arguments.of("testApiWithNoComponents", OpenApiGeneratorFixtures.TEST_API_WITH_NO_COMPONENTS,
            OpenApiGeneratorFixtures.validateApiWithNoComponents()),
        Arguments.of("testRestrictionSchema", OpenApiGeneratorFixtures.TEST_RESTRICTION_SCHEMA,
            OpenApiGeneratorFixtures.validateRestrictionsSchema()),
        Arguments.of("testSimpleBuild", OpenApiGeneratorFixtures.TEST_SIMPLE_BUILD,
            OpenApiGeneratorFixtures.validateSimpleBuild()),
        Arguments.of("testFormDataMultipartGeneration", OpenApiGeneratorFixtures.TEST_FORM_DATA_MULTIPART_GENERATION,
            OpenApiGeneratorFixtures.validateDataMultipartGeneration()),
        Arguments.of("testConsumerSpecNoServers", OpenApiGeneratorFixtures.TEST_CONSUMER_SPEC_NO_SERVERS,
            OpenApiGeneratorFixtures.validateConsumerSpecNoServers())
    );
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpec(final String type, final List<SpecFile> specFileList, final Function<Path, Boolean> validation) {
    openApiGenerator.processFileSpec(specFileList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }

  /**
   * Numeric restrictions ({@code @Size}, {@code @MaxItems}, {@code @MinItems}) must be rendered as plain Java integer
   * literals regardless of the default JVM locale. Under a locale with a grouping separator, values >= 1000 used to be
   * written as {@code 4.000} / {@code 4,000}, which does not compile. See issue #420.
   */
  @Test
  void processFileSpecIsIndependentOfDefaultLocale() {
    final Locale previousLocale = Locale.getDefault();
    try {
      Locale.setDefault(Locale.forLanguageTag("es-ES"));
      openApiGenerator.processFileSpec(OpenApiGeneratorFixtures.TEST_VALIDATION_ANNOTATIONS);
      Assertions.assertThat(OpenApiGeneratorFixtures.validateValidationAnnotations(SPRING_BOOT_VERSION).apply(baseDir)).isTrue();
    } finally {
      Locale.setDefault(previousLocale);
    }
  }

  @Test
  void testExceptionForTestGenerationWithNoOperationId() {
    Assertions.assertThatThrownBy(() -> openApiGenerator.processFileSpec(OpenApiGeneratorFixtures.TEST_GENERATION_WITH_NO_OPERATION_ID))
        .isInstanceOf(InvalidAPIException.class);
  }
}
