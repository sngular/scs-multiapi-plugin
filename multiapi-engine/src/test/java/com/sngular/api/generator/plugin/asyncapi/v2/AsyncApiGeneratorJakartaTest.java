/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.v2;

import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import com.sngular.api.generator.plugin.asyncapi.AsyncApiGenerator;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.io.CleanupMode;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class AsyncApiGeneratorJakartaTest {

  @TempDir(cleanup = CleanupMode.ON_SUCCESS)
  static Path baseDir;

  private static AsyncApiGenerator asyncApiGenerator;

  private static final int SPRING_BOOT_VERSION = 3;

  @BeforeAll
  static void setup() {
    asyncApiGenerator =
        new AsyncApiGenerator(SPRING_BOOT_VERSION, true, new File(baseDir.toAbsolutePath() + File.separator + AsyncApiGeneratorFixtures.TARGET), AsyncApiGeneratorFixtures.GENERATED,
            "groupId",
            baseDir.toFile());
  }

  static Stream<Arguments> fileSpecToProcess() {
    return Stream.of(
        Arguments.of("TestCustomValidators", AsyncApiGeneratorFixtures.TEST_CUSTOM_VALIDATORS, AsyncApiGeneratorFixtures.validateCustomValidators(SPRING_BOOT_VERSION))
    );
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpecForTestFileGeneration(final String type, final List<SpecFile> specFileList, final Function<Path, Boolean> validation) {
    asyncApiGenerator.processFileSpec(specFileList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }

}
