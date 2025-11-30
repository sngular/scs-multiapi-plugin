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
import org.junit.jupiter.api.io.CleanupMode;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class OpenApiGeneratorJakartaTest {

  private static final int SPRING_BOOT_VERSION = 3;
  @TempDir(cleanup = CleanupMode.NEVER)
  static Path baseDir;
  private static OpenApiGenerator openApiGenerator;

  @BeforeAll
  static void setup() {
    openApiGenerator =
        new OpenApiGenerator(SPRING_BOOT_VERSION, Boolean.TRUE, new File(baseDir.toAbsolutePath() + File.separator + OpenApiGeneratorFixtures.TARGET), OpenApiGeneratorFixtures.GENERATED, "groupId",
            baseDir.toFile());
  }

  static Stream<Arguments> fileSpecToProcess() {
    return Stream.of(
        Arguments.of("testApiReactiveGeneration", OpenApiGeneratorFixtures.TEST_API_REACTIVE_GENERATION,
            OpenApiGeneratorFixtures.validateApiReactiveGeneration(SPRING_BOOT_VERSION)),
        Arguments.of("testValidationAnnotations", OpenApiGeneratorFixtures.TEST_VALIDATION_ANNOTATIONS,
            OpenApiGeneratorFixtures.validateValidationAnnotations(SPRING_BOOT_VERSION)),
        Arguments.of("testValidationAnnotationsLombok", OpenApiGeneratorFixtures.TEST_VALIDATION_ANNOTATIONS_LOMBOK,
            OpenApiGeneratorFixtures.validateValidationAnnotationsLombok(SPRING_BOOT_VERSION))
    );
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpec(final String type, final List<SpecFile> specFileList, final Function<Path, Boolean> validation) {
    openApiGenerator.processFileSpec(specFileList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }
}