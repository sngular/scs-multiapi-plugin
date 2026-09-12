/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
package com.sngular.api.generator.plugin;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Properties;
import java.util.regex.Pattern;

class PluginIntegrationTestBase {

  static void copyResourceDirectory(final String resource, final Path targetDir) throws Exception {
    final URL resourceUrl = PluginIntegrationTestBase.class.getResource(resource);
    assertThat(resourceUrl).isNotNull();
    final Path sourceDir = Path.of(resourceUrl.toURI());
    try (final var paths = Files.walk(sourceDir)) {
      for (final Path source : paths.toList()) {
        final Path target = targetDir.resolve(sourceDir.relativize(source).toString());
        if (Files.isDirectory(source)) {
          Files.createDirectories(target);
        } else {
          Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
        }
      }
    }
  }

  static void setAbsoluteSpecPath(final Path projectDir, final String specFileName) throws IOException {
    final Path buildFile = projectDir.resolve("build.gradle");
    final String content = Files.readString(buildFile, StandardCharsets.UTF_8)
        .replace("PLACEHOLDER_FILEPATH", projectDir.resolve(specFileName).toAbsolutePath().toString());
    Files.writeString(buildFile, content, StandardCharsets.UTF_8);
  }

  static List<File> pluginClasspath() {
    final String metadataFile = System.getProperty("plugin-under-test-metadata-file");
    if (metadataFile == null) {
      throw new IllegalStateException("plugin-under-test-metadata-file system property is not set");
    }
    final Properties properties = new Properties();
    try (var in = Files.newInputStream(Path.of(metadataFile))) {
      properties.load(in);
    } catch (IOException e) {
      throw new IllegalStateException("Could not read plugin-under-test metadata", e);
    }
    final String classpath = properties.getProperty("implementation-classpath");
    assertThat(classpath).isNotNull();
    return java.util.Arrays.stream(classpath.split(Pattern.quote(File.pathSeparator)))
        .map(File::new)
        .toList();
  }
}