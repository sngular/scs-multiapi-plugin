/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
package com.sngular.api.generator.plugin;

import static com.sngular.api.generator.plugin.PluginIntegrationTestBase.copyResourceDirectory;
import static com.sngular.api.generator.plugin.PluginIntegrationTestBase.pluginClasspath;
import static com.sngular.api.generator.plugin.PluginIntegrationTestBase.setAbsoluteSpecPath;
import static org.assertj.core.api.Assertions.assertThat;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.gradle.testkit.runner.BuildResult;
import org.gradle.testkit.runner.GradleRunner;
import org.gradle.testkit.runner.TaskOutcome;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class OpenApiPluginIntegrationTest {

  @TempDir
  Path testProjectDir;

  @Test
  void openApiTaskGeneratesSources() throws Exception {
    copyResourceDirectory("/test-project-openapi", testProjectDir);
    setAbsoluteSpecPath(testProjectDir, "openapi.yml");

    final BuildResult result = GradleRunner.create()
        .withProjectDir(testProjectDir.toFile())
        .withArguments("openApiTask")
        .withPluginClasspath(pluginClasspath())
        .build();
    assertThat(result.task(":openApiTask").getOutcome()).isEqualTo(TaskOutcome.SUCCESS);
    final Path generatedDir = testProjectDir.resolve("build/generated-source");
    assertThat(generatedDir).isDirectory();
    try (final var paths = Files.walk(generatedDir)) {
      final List<Path> javaFiles = paths.filter(p -> p.toString().endsWith(".java")).toList();
      assertThat(javaFiles).isNotEmpty();
      assertThat(javaFiles).anyMatch(p -> p.getFileName().toString().equals("Test.java"));
    }
  }

  @Test
  void openApiTaskIsUpToDateOnSecondRun() throws Exception {
    copyResourceDirectory("/test-project-openapi", testProjectDir);
    setAbsoluteSpecPath(testProjectDir, "openapi.yml");

    GradleRunner.create()
        .withProjectDir(testProjectDir.toFile())
        .withArguments("openApiTask")
        .withPluginClasspath(pluginClasspath())
        .build();

    final BuildResult secondRun = GradleRunner.create()
        .withProjectDir(testProjectDir.toFile())
        .withArguments("openApiTask")
        .withPluginClasspath(pluginClasspath())
        .build();

    assertThat(secondRun.task(":openApiTask").getOutcome()).isEqualTo(TaskOutcome.UP_TO_DATE);
  }
}
