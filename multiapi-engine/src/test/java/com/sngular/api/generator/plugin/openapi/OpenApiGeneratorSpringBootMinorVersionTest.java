/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * {@code springBootVersion} as {@code MAJOR.MINOR}: features that need a later minor are only generated for projects on it.
 * {@code RestClient} is part of Spring Framework 6.1, i.e. Spring Boot 3.2.
 */
class OpenApiGeneratorSpringBootMinorVersionTest {

  @TempDir
  Path baseDir;

  @ParameterizedTest(name = "springBootVersion {0} -> RestClient constructor: {1}")
  @CsvSource({"3, false", "3.1, false", "3.2, true", "4, true"})
  void restClientIsInjectableFromSpringBoot32(final String springBootVersion, final boolean expectRestClient) throws IOException {
    final Path target = baseDir.resolve("v" + springBootVersion);
    new OpenApiGenerator(springBootVersion, Boolean.TRUE, target.toFile(), OpenApiGeneratorFixtures.GENERATED, "groupId",
        Path.of("src", "test", "resources").toFile())
        .processFileSpec(List.of(OpenApiGeneratorFixtures.TEST_REST_CLIENT_WITHOUT_COMPONENT.get(0)));

    final String apiRestClient = Files.readString(target.resolve(OpenApiGeneratorFixtures.GENERATED)
                                                        .resolve("com/sngular/multifileplugin/restclientnocomponent/client/ApiRestClient.java"));

    Assertions.assertThat(apiRestClient.contains("public ApiRestClient(final RestClient restClient)")).isEqualTo(expectRestClient);
    Assertions.assertThat(apiRestClient.contains("import org.springframework.web.client.RestClient;")).isEqualTo(expectRestClient);
  }
}
