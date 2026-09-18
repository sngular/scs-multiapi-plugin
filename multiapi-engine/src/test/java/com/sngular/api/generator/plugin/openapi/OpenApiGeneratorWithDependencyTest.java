/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import com.sngular.api.generator.plugin.common.loader.SpecArtifactResolver;
import com.sngular.api.generator.plugin.exception.SpecDependencyException;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Generation driven from a contract published inside an artifact rather than checked into the
 * module, and the conventional contract location that lets a spec declare no {@code filePath} at
 * all. The fixture is a multi-file contract on purpose: the root document only names the schemas,
 * so a generated model class proves the nested {@code $ref}s were resolved inside the artifact
 * too.
 */
@DisplayName("Resolving where a contract lives")
class OpenApiGeneratorWithDependencyTest {

  private static final int SPRING_BOOT_VERSION = 2;

  private static final String SPEC_FOLDER = "contracts";

  private static final String SPEC_PATH = SPEC_FOLDER + "/api-test.yml";

  private static final String API_PACKAGE = "com.sngular.multifileplugin.fromdependency";

  private static final Path FIXTURE = Paths.get("src", "test", "resources", "openapigenerator", "testNestedExternalRefs");

  @TempDir
  Path tempDir;

  @Test
  @DisplayName("generates the API and the models referenced across files inside the artifact")
  void generatesFromArtifact() throws IOException {
    final File artifact = specArtifact();
    final Path targetFolder = tempDir.resolve("target");

    generatorFor(artifact, targetFolder).processFileSpec(List.of(specFileBuilder(SPEC_PATH).build()));

    final Path api = targetFolder.resolve("generated/com/sngular/multifileplugin/fromdependency");
    assertThat(api.resolve("ServicesApi.java")).isRegularFile();
    assertThat(api.resolve("model/InlineResponse200ListServicesDTO.java")).isRegularFile();
    assertThat(api.resolve("model/Service_typeDTO.java")).isRegularFile();
  }

  @Test
  @DisplayName("unpacks the artifact under the build directory instead of polluting the module")
  void extractsUnderTheBuildDirectory() throws IOException {
    final File artifact = specArtifact();
    final Path targetFolder = tempDir.resolve("target");

    generatorFor(artifact, targetFolder).processFileSpec(List.of(specFileBuilder(SPEC_PATH).build()));

    assertThat(targetFolder.resolve("generated-resources/multiapi-specs/com.company/api-specs")).isDirectoryContaining(
        entry -> Files.isRegularFile(entry.resolve(SPEC_PATH)));
  }

  @Test
  @DisplayName("fails naming the spec and the artifact when the path inside it is wrong")
  void failsWithAnActionableMessage() throws IOException {
    final File artifact = specArtifact();
    final var generator = generatorFor(artifact, tempDir.resolve("target"));
    final List<SpecFile> specFiles = List.of(specFileBuilder("openapi/openapi.yml").build());

    assertThatThrownBy(() -> generator.processFileSpec(specFiles))
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("openapi/openapi.yml")
        .hasMessageContaining("com.company:api-specs:1.0.0")
        .hasMessageContaining(SPEC_PATH);
  }

  @Test
  @DisplayName("falls back to contract/openapi.yml in the module when nothing is configured")
  void defaultsToTheConventionalPathInTheModule() throws IOException {
    final Path moduleDir = Files.createDirectories(tempDir.resolve("module"));
    copyFixtureInto(moduleDir.resolve("contract"), "openapi.yml");
    final Path targetFolder = tempDir.resolve("target");

    final var generator = new OpenApiGenerator(SPRING_BOOT_VERSION, Boolean.TRUE, targetFolder.toFile(), "generated",
        "groupId", moduleDir.toFile());
    generator.processFileSpec(List.of(SpecFile.builder()
                                              .apiPackage(API_PACKAGE)
                                              .modelPackage(API_PACKAGE + ".model")
                                              .modelNameSuffix("DTO")
                                              .useLombokModelAnnotation(true)
                                              .build()));

    final Path api = targetFolder.resolve("generated/com/sngular/multifileplugin/fromdependency");
    assertThat(api.resolve("ServicesApi.java")).isRegularFile();
    assertThat(api.resolve("model/Service_typeDTO.java")).isRegularFile();
  }

  @Test
  @DisplayName("still reads the filesystem when no dependency coordinates are given")
  void keepsReadingFromTheFilesystemWithoutCoordinates() {
    final Path targetFolder = tempDir.resolve("target");
    final var generator = new OpenApiGenerator(SPRING_BOOT_VERSION, Boolean.TRUE, targetFolder.toFile(), "generated",
        "groupId", Paths.get("src", "test", "resources").toFile());

    generator.processFileSpec(List.of(SpecFile.builder()
                                              .filePath("openapigenerator/testNestedExternalRefs/api-test.yml")
                                              .apiPackage(API_PACKAGE)
                                              .modelPackage(API_PACKAGE + ".model")
                                              .modelNameSuffix("DTO")
                                              .useLombokModelAnnotation(true)
                                              .build()));

    assertThat(targetFolder.resolve("generated/com/sngular/multifileplugin/fromdependency/ServicesApi.java")).isRegularFile();
    assertThat(targetFolder.resolve("generated-resources/multiapi-specs")).doesNotExist();
  }

  private OpenApiGenerator generatorFor(final File artifact, final Path targetFolder) throws IOException {
    final Path moduleDir = Files.createDirectories(tempDir.resolve("module"));
    final var generator = new OpenApiGenerator(SPRING_BOOT_VERSION, Boolean.TRUE, targetFolder.toFile(), "generated",
        "groupId", moduleDir.toFile());
    final SpecArtifactResolver resolver = (groupId, artifactId, version) -> artifact;
    generator.setArtifactResolver(resolver);
    return generator;
  }

  private static SpecFile.SpecFileBuilder<?, ?> specFileBuilder(final String filePath) {
    return SpecFile.builder()
                   .filePath(filePath)
                   .fromGroupId("com.company")
                   .fromArtifactId("api-specs")
                   .fromVersion("1.0.0")
                   .apiPackage(API_PACKAGE)
                   .modelPackage(API_PACKAGE + ".model")
                   .modelNameSuffix("DTO")
                   .useLombokModelAnnotation(true);
  }

  /** Copies the multi-file fixture contract into {@code destination}, renaming its root document. */
  private static void copyFixtureInto(final Path destination, final String rootName) throws IOException {
    Files.createDirectories(destination);
    try (var fixtureFiles = Files.walk(FIXTURE)) {
      for (final Path file : fixtureFiles.filter(Files::isRegularFile).toList()) {
        final String relative = FIXTURE.relativize(file).toString().replace(File.separatorChar, '/');
        if (relative.startsWith("assets/")) {
          continue;
        }
        final Path target = destination.resolve("api-test.yml".equals(relative) ? rootName : relative);
        Files.createDirectories(target.getParent());
        Files.copy(file, target);
      }
    }
  }

  /**
   * Packages the multi-file fixture contract into a JAR, mirroring how a team publishes its API
   * contracts as an ordinary artifact.
   */
  private File specArtifact() throws IOException {
    final File artifact = tempDir.resolve("api-specs-1.0.0.jar").toFile();
    try (OutputStream out = Files.newOutputStream(artifact.toPath());
        ZipOutputStream zip = new ZipOutputStream(out);
        var fixtureFiles = Files.walk(FIXTURE)) {
      for (final Path file : fixtureFiles.filter(Files::isRegularFile).toList()) {
        final String relative = FIXTURE.relativize(file).toString().replace(File.separatorChar, '/');
        if (relative.startsWith("assets/")) {
          continue;
        }
        zip.putNextEntry(new ZipEntry(SPEC_FOLDER + "/" + relative));
        zip.write(Files.readAllBytes(file));
        zip.closeEntry();
      }
    }
    return artifact;
  }
}
