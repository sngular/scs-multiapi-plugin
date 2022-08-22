package net.coru.api.generator.plugin.openapi;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
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
      Arguments.of("testAllOf", OpenApiGeneratorFixtures.TEST_ALL_OF, OpenApiGeneratorFixtures.VALIDATE_ALL_OF()),
      Arguments.of("testAllOfLombok", OpenApiGeneratorFixtures.TEST_ALL_OF_LOMBOK, OpenApiGeneratorFixtures.VALIDATE_ALL_OF_LOMBOK()),
      Arguments.of("testOverWriteModelTrue", OpenApiGeneratorFixtures.TEST_OVER_WRITE_MODEL_TRUE, OpenApiGeneratorFixtures.VALIDATE_OVERWRITE_MODE_TRUE()),
      Arguments.of("testApiClientGeneration", OpenApiGeneratorFixtures.TEST_API_CLIENT_GENERATION, OpenApiGeneratorFixtures.VALIDATE_API_CLIENT_GENERATION()),
      Arguments.of("testInlineSchemaCreation", OpenApiGeneratorFixtures.TEST_INLINE_SCHEMA_CREATION, OpenApiGeneratorFixtures.VALIDATE_INLINE_SCHEMA_CREATION()),
      Arguments.of("testApiParametersWithContentGeneration", OpenApiGeneratorFixtures.TEST_PARAMETER_WITH_CONTENT_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_PARAMETERS_WITH_SCHEMA_GENERATION()),
      Arguments.of("testApiPathWithSpecialCharGeneration", OpenApiGeneratorFixtures.TEST_PATH_WITH_SLASH_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_PATH_WITH_SPECIAL_CHAR_GENERATION()),
      Arguments.of("testApiReactiveGeneration",OpenApiGeneratorFixtures.TEST_API_REACTIVE_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_API_REACTIVE_GENERATION()),
      Arguments.of("testApiTagsGeneration",OpenApiGeneratorFixtures.TEST_API_TAGS_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_TAGS_GENERATION()),
      Arguments.of("testMultipleRefGeneration",OpenApiGeneratorFixtures.TEST_MULTIPLE_REF_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_MULTIPLE_REF_GENERATION()),
      Arguments.of("testApiPathParameterGeneration",OpenApiGeneratorFixtures.TEST_PATH_PARAMETER_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_PATH_PARAMETER_GENERATION()),
      Arguments.of("testWebClientApiGeneration",OpenApiGeneratorFixtures.TEST_WEB_CLIENT_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_WEB_CLIENT_GENERATION()),
      Arguments.of("testClientPackageWebClientApiGeneration",OpenApiGeneratorFixtures.TEST_CLIENT_PACKAGE_WEB_CLIENT_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_CLIENT_PACKAGE_WEB_CLIENT_GENERATION()),
      Arguments.of("testRestClientApiGeneration",OpenApiGeneratorFixtures.TEST_REST_CLIENT_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_REST_CLIENT_GENERATION()),
      Arguments.of("testApiEnumsGeneration",OpenApiGeneratorFixtures.TEST_ENUMS_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_ENUMS_GENERATION()),
      Arguments.of("testApiEnumsLombokGeneration",OpenApiGeneratorFixtures.TEST_ENUMS_LOMBOK_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_ENUMS_LOMBOK_GENERATION()),
      Arguments.of("testExternalRefsGeneration",OpenApiGeneratorFixtures.TEST_EXTERNAL_REF_GENERATION,
                   OpenApiGeneratorFixtures.VALIDATE_EXTERNAL_REF_GENERATION()),
      Arguments.of("testAnyOfInResponse",OpenApiGeneratorFixtures.TEST_ANY_OF_IN_RESPONSE,
                   OpenApiGeneratorFixtures.VALIDATE_ANY_OF_IN_RESPONSE()),
      Arguments.of("testOneOfInResponse",OpenApiGeneratorFixtures.TEST_ONE_OF_IN_RESPONSE,
                   OpenApiGeneratorFixtures.VALIDATE_ONE_OF_IN_RESPONSE())
    );
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpec(final String type, final List<FileSpec> fileSpecList, final Function<Path, Boolean> validation) {
    openApiGenerator.processFileSpec(fileSpecList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }
}