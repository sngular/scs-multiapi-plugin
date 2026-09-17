/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin;

import static com.sngular.api.generator.plugin.PluginIntegrationTestBase.copyResourceDirectory;
import static com.sngular.api.generator.plugin.PluginIntegrationTestBase.pluginClasspath;
import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.gradle.testkit.runner.BuildResult;
import org.gradle.testkit.runner.GradleRunner;
import org.gradle.testkit.runner.TaskOutcome;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Generation driven from a contract published as an artifact. The contract is packaged into a
 * repository outside the project and its sources are removed from the project directory, so the
 * build can only succeed by resolving and unpacking the artifact.
 */
class OpenApiPluginDependencyIntegrationTest {

  private static final String SPEC_SOURCES = "spec-artifact";

  @TempDir
  private Path testProjectDir;

  @Test
  void openApiTaskGeneratesSourcesFromAnArtifact() throws Exception {
    prepareProject();

    final BuildResult result = GradleRunner.create()
        .withProjectDir(testProjectDir.toFile())
        .withArguments("openApiTask")
        .withPluginClasspath(pluginClasspath())
        .build();

    assertThat(result.task(":openApiTask").getOutcome()).isEqualTo(TaskOutcome.SUCCESS);
    final Path generatedDir = testProjectDir.resolve("build/generated-source");
    try (final var paths = Files.walk(generatedDir)) {
      final List<String> generated = paths.map(path -> path.getFileName().toString())
                                          .filter(name -> name.endsWith(".java"))
                                          .toList();
      // Address.java comes from a $ref nested two files deep inside the artifact, so its presence
      // is what proves the whole contract was resolved and not just the root document.
      assertThat(generated).contains("TestApi.java", "InlineResponse200TestCreateDTO.java", "Address.java");
    }
  }

  @Test
  void openApiTaskFailsNamingTheArtifactWhenThePathInsideItIsWrong() throws Exception {
    prepareProject();
    replaceInBuildFile("contracts/api.yml", "contracts/missing.yml");

    final BuildResult result = GradleRunner.create()
        .withProjectDir(testProjectDir.toFile())
        .withArguments("openApiTask")
        .withPluginClasspath(pluginClasspath())
        .buildAndFail();

    assertThat(result.getOutput()).contains("contracts/missing.yml")
                                  .contains("com.company:api-specs:1.0.0")
                                  .contains("contracts/api.yml");
  }

  /**
   * Publishes the fixture contract as an artifact in a repository of its own and deletes the
   * contract from the project, leaving the artifact as the only possible source.
   */
  private void prepareProject() throws Exception {
    copyResourceDirectory("/test-project-openapi-dependency", testProjectDir);

    final Path specSources = testProjectDir.resolve(SPEC_SOURCES);
    final Path repository = testProjectDir.resolve("repository/com/company/api-specs/1.0.0");
    Files.createDirectories(repository);
    packageArtifact(specSources, repository.resolve("api-specs-1.0.0.jar"));
    Files.writeString(repository.resolve("api-specs-1.0.0.pom"), pom(), StandardCharsets.UTF_8);
    deleteRecursively(specSources);

    replaceInBuildFile("PLACEHOLDER_REPOSITORY", testProjectDir.resolve("repository").toUri().toString());
  }

  private void replaceInBuildFile(final String placeholder, final String replacement) throws IOException {
    final Path buildFile = testProjectDir.resolve("build.gradle");
    Files.writeString(buildFile, Files.readString(buildFile, StandardCharsets.UTF_8).replace(placeholder, replacement), StandardCharsets.UTF_8);
  }

  private static void packageArtifact(final Path specSources, final Path artifact) throws IOException {
    try (OutputStream out = Files.newOutputStream(artifact);
        ZipOutputStream zip = new ZipOutputStream(out);
        var files = Files.walk(specSources)) {
      for (final Path file : files.filter(Files::isRegularFile).toList()) {
        zip.putNextEntry(new ZipEntry(specSources.relativize(file).toString().replace(File.separatorChar, '/')));
        zip.write(Files.readAllBytes(file));
        zip.closeEntry();
      }
    }
  }

  private static String pom() {
    return """
        <?xml version="1.0" encoding="UTF-8"?>
        <project xmlns="http://maven.apache.org/POM/4.0.0">
          <modelVersion>4.0.0</modelVersion>
          <groupId>com.company</groupId>
          <artifactId>api-specs</artifactId>
          <version>1.0.0</version>
        </project>
        """;
  }

  private static void deleteRecursively(final Path directory) throws IOException {
    try (var paths = Files.walk(directory)) {
      for (final Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
        Files.delete(path);
      }
    }
  }
}
