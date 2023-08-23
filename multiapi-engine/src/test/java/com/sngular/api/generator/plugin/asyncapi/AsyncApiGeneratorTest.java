/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.exception.InvalidAPIException;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.CleanupMode;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class AsyncApiGeneratorTest {

  @TempDir(cleanup = CleanupMode.NEVER)
  static Path baseDir;

  private static AsyncApiGenerator asyncApiGenerator;

  private static final int SPRING_BOOT_VERSION = 2;

  @BeforeAll
  static void setup() {
    asyncApiGenerator =
        new AsyncApiGenerator(SPRING_BOOT_VERSION, new File(baseDir.toAbsolutePath() + File.separator + AsyncApiGeneratorFixtures.TARGET), AsyncApiGeneratorFixtures.GENERATED,
                              "groupId",
                              baseDir.toFile());
  }

  static Stream<Arguments> fileSpecToProcess() {
    return Stream.of(
        Arguments.of("TestFileGeneration", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION, AsyncApiGeneratorFixtures.validateTestFileGeneration()),
        Arguments.of("TestIssueGeneration", AsyncApiGeneratorFixtures.TEST_ISSUE_GENERATION, AsyncApiGeneratorFixtures.validateTestIssueGeneration()),
        Arguments.of("TestFileGenerationIssue", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_ISSUE, AsyncApiGeneratorFixtures.validateTestFileGenerationIssue()),
        Arguments.of("TestFileGenerationExternalAvro", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_EXTERNAL_AVRO,
                     AsyncApiGeneratorFixtures.validateTestFileGenerationExternalAvro()),
        Arguments.of("TestFileGenerationStreamBridge", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_STREAM_BRIDGE,
                     AsyncApiGeneratorFixtures.validateTestFileGenerationStreamBridge()),
        Arguments.of("TestFileGenerationWithoutIds", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_WITHOUT_IDS,
                     AsyncApiGeneratorFixtures.validateTestFileGenerationWithoutIds()),
        Arguments.of("TestFileGenerationArrayString", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_WITH_ARRAY_STRING,
                     AsyncApiGeneratorFixtures.validateTestFileGenerationArrayString()),
        Arguments.of("TestIssueGenerateSupplier", AsyncApiGeneratorFixtures.TEST_ISSUE_GENERATE_SUPPLIER,
                     AsyncApiGeneratorFixtures.validateTestIssueGenerateSupplier()),
        Arguments.of("TestIssueInfiniteLoop", AsyncApiGeneratorFixtures.TEST_ISSUE_INFINITE_LOOP,
                     AsyncApiGeneratorFixtures.validateTestIssueInfiniteLoop()),
        Arguments.of("TestCustomValidators", AsyncApiGeneratorFixtures.TEST_CUSTOM_VALIDATORS, AsyncApiGeneratorFixtures.validateCustomValidators(SPRING_BOOT_VERSION)),
        Arguments.of("TestModelClassExceptionGeneration", AsyncApiGeneratorFixtures.TEST_MODEL_CLASS_EXCEPTION_GENERATION,
                     AsyncApiGeneratorFixtures.validateTestModelClassExceptionGeneration()),
        Arguments.of("TestNoSchemas", AsyncApiGeneratorFixtures.TEST_NO_SCHEMAS, AsyncApiGeneratorFixtures.validateNoSchemas()),
        Arguments.of("TestNestedObjectIssue", AsyncApiGeneratorFixtures.TEST_NESTED_OBJECT, AsyncApiGeneratorFixtures.validateNestedObject()),
        Arguments.of("TestFileGenerationWithKafkaBindings", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_WITH_KAFKA_BINDINGS,
                     AsyncApiGeneratorFixtures.validateTestFileGenerationWithKafkaBindings())

    );
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpecForTestFileGeneration(final String type, final List<SpecFile> specFileList, final Function<Path, Boolean> validation) {
    asyncApiGenerator.processFileSpec(specFileList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }

  @Test
  void testExceptionForTestGenerationWithNoOperationId() {
    Assertions.assertThatThrownBy(() -> asyncApiGenerator.processFileSpec(AsyncApiGeneratorFixtures.TEST_GENERATION_WITH_NO_OPERATION_ID))
      .isInstanceOf(InvalidAPIException.class);
  }

  @Test
  void testExceptionForTestGenerationWithNoOperationConfiguration() {
    Assertions.assertThatThrownBy(() -> asyncApiGenerator.processFileSpec(AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_NO_CONFIG))
      .isInstanceOf(InvalidAPIException.class);
  }

}
