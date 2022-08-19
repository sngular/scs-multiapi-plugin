/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import net.coru.api.generator.plugin.asyncapi.parameter.FileSpec;
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
        Arguments.of("TestFileGeneration", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION, AsyncApiGeneratorFixtures.VALIDATE_TEST_FILE_GENERATION()),
        Arguments.of("TestFileGenerationExternalAvro", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_EXTERNAL_AVRO,
                     AsyncApiGeneratorFixtures.VALIDATE_TEST_FILE_GENERATION_EXTERNAL_AVRO()),
        Arguments.of("TestFileGenerationStreamBridge", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_STREAM_BRIDGE,
                     AsyncApiGeneratorFixtures.VALIDATE_TEST_FILE_GENERATION_STREAM_BRIDGE()),
        Arguments.of("TestFileGenerationWithoutIds", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_WITHOUT_IDS,
                     AsyncApiGeneratorFixtures.VALIDATE_TEST_FILE_GENERATION_WITHOUT_IDS())
    );
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpecForTestFileGeneration(final String type, final List<FileSpec> fileSpecList, final Function<Path, Boolean> validation) {
    asyncApiGenerator.processFileSpec(fileSpecList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }

}
