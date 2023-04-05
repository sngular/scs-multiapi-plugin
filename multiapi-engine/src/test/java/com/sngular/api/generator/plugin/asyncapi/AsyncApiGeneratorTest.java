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
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class AsyncApiGeneratorTest {

  @TempDir
  static Path baseDir;

  private static AsyncApiGenerator asyncApiGenerator;

  @BeforeAll
  static void setup() {
    asyncApiGenerator =
        new AsyncApiGenerator(new File(baseDir.toAbsolutePath() + File.separator + AsyncApiGeneratorFixtures.TARGET), AsyncApiGeneratorFixtures.GENERATED, "groupId",
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
        Arguments.of("TestCustomValidators", AsyncApiGeneratorFixtures.TEST_CUSTOM_VALIDATORS, AsyncApiGeneratorFixtures.validateCustomValidators())
    );
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpecForTestFileGeneration(final String type, final List<SpecFile> specFileList, final Function<Path, Boolean> validation) {
    asyncApiGenerator.processFileSpec(specFileList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }

}
