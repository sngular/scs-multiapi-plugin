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

/**
 * Spring Boot 4 (Spring Framework 7 + Jackson 3) code generation. Verifies that the generated
 * clients import {@code tools.jackson.*} databind classes, build the immutable {@code JsonMapper},
 * use the Spring 7 reactive codecs, while Jackson annotations stay on {@code com.fasterxml.jackson.annotation}.
 */
class OpenApiGeneratorSpringBoot4Test {

  private static final int SPRING_BOOT_VERSION = 4;

  @TempDir(cleanup = CleanupMode.NEVER)
  static Path baseDir;

  private static OpenApiGenerator openApiGenerator;

  @BeforeAll
  static void setup() {
    openApiGenerator =
        new OpenApiGenerator(SPRING_BOOT_VERSION, Boolean.TRUE, new File(baseDir.toAbsolutePath() + File.separator + OpenApiGeneratorFixtures.TARGET),
            OpenApiGeneratorFixtures.GENERATED, "groupId", Path.of("src", "test", "resources").toFile());
  }

  static Stream<Arguments> fileSpecToProcess() {
    return Stream.of(
        Arguments.of("testRestClientApiGeneration", OpenApiGeneratorFixtures.TEST_REST_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateRestClientGenerationSpringBoot4()),
        Arguments.of("testWebClientApiGeneration", OpenApiGeneratorFixtures.TEST_WEB_CLIENT_GENERATION,
            OpenApiGeneratorFixtures.validateWebClientGenerationSpringBoot4()),
        Arguments.of("testHttpExchangeClient", OpenApiGeneratorFixtures.TEST_HTTP_EXCHANGE_CLIENT,
            OpenApiGeneratorFixtures.validateHttpExchangeClient("httpexchange", "imperative")),
        Arguments.of("testHttpExchangeClientReactive", OpenApiGeneratorFixtures.TEST_HTTP_EXCHANGE_CLIENT_REACTIVE,
            OpenApiGeneratorFixtures.validateHttpExchangeClient("httpexchangereactive", "reactive"))
    );
  }

  @ParameterizedTest(name = "Test {index} - Spring Boot 4 File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpec(final String type, final List<SpecFile> specFileList, final Function<Path, Boolean> validation) {
    openApiGenerator.processFileSpec(specFileList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }
}
