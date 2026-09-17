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

class AsyncApiPluginIntegrationTest {

  @TempDir
  private Path testProjectDir;

  @Test
  void asyncApiTaskGeneratesSources() throws Exception {
    copyResourceDirectory("/test-project-asyncapi", testProjectDir);
    setAbsoluteSpecPath(testProjectDir, "event-api.yml");

    final BuildResult result = GradleRunner.create()
        .withProjectDir(testProjectDir.toFile())
        .withArguments("asyncApiTask")
        .withPluginClasspath(pluginClasspath())
        .build();

    assertThat(result.task(":asyncApiTask").getOutcome()).isEqualTo(TaskOutcome.SUCCESS);
    final Path generatedDir = testProjectDir.resolve("build/generated-source");
    assertThat(generatedDir).isDirectory();
    try (final var paths = Files.walk(generatedDir)) {
      final List<Path> javaFiles = paths.filter(p -> p.toString().endsWith(".java")).toList();
      assertThat(javaFiles).isNotEmpty();
    }
  }

  @Test
  void asyncApiTaskIsUpToDateOnSecondRun() throws Exception {
    copyResourceDirectory("/test-project-asyncapi", testProjectDir);
    setAbsoluteSpecPath(testProjectDir, "event-api.yml");

    GradleRunner.create()
        .withProjectDir(testProjectDir.toFile())
        .withArguments("asyncApiTask")
        .withPluginClasspath(pluginClasspath())
        .build();

    final BuildResult secondRun = GradleRunner.create()
        .withProjectDir(testProjectDir.toFile())
        .withArguments("asyncApiTask")
        .withPluginClasspath(pluginClasspath())
        .build();

    assertThat(secondRun.task(":asyncApiTask").getOutcome()).isEqualTo(TaskOutcome.UP_TO_DATE);
  }
}
