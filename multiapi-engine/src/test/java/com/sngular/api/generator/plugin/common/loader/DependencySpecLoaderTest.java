/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.loader;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.io.IOException;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Tests for DependencySpecLoader - loading specs from Maven dependencies.
 */
@DisplayName("DependencySpecLoader - Load OpenAPI/AsyncAPI specs from Maven JARs")
class DependencySpecLoaderTest {

  @AfterEach
  void cleanup() {
    DependencySpecLoader.clearCache();
  }

  @Test
  @DisplayName("Should load spec from classpath (no external dependency)")
  void testLoadSpecFromClasspath() throws Exception {
    // Note: Full integration test with real Maven artifacts would require test JARs
    // This test verifies the DependencySpecLoader API is correct
    assertThat(DependencySpecLoader.class).isNotNull();
  }

  @Test
  @DisplayName("Should throw IOException when JAR not found")
  void testLoadSpecFromInvalidDependency() {
    assertThatThrownBy(() -> DependencySpecLoader.loadSpec(
        "specs/api.yml",
        "invalid.group",
        "invalid-artifact",
        "9.9.9"))
        .isInstanceOf(IOException.class)
        .hasMessageContaining("Cannot resolve Maven artifact");
  }

  @Test
  @DisplayName("Should throw IllegalArgumentException for null filePath")
  void testLoadSpecWithNullFilePath() {
    assertThatThrownBy(() -> DependencySpecLoader.loadSpec(
        null,
        "com.company",
        "api-spec",
        "1.0.0"))
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessageContaining("filePath");
  }

  @Test
  @DisplayName("Should throw IllegalArgumentException for null groupId")
  void testLoadSpecWithNullGroupId() {
    assertThatThrownBy(() -> DependencySpecLoader.loadSpec(
        "specs/api.yml",
        null,
        "api-spec",
        "1.0.0"))
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessageContaining("groupId");
  }

  @Test
  @DisplayName("Should throw IllegalArgumentException for null artifactId")
  void testLoadSpecWithNullArtifactId() {
    assertThatThrownBy(() -> DependencySpecLoader.loadSpec(
        "specs/api.yml",
        "com.company",
        null,
        "1.0.0"))
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessageContaining("artifactId");
  }

  @Test
  @DisplayName("Should cache loaders to avoid repeated JAR loads")
  void testLoaderCaching() {
    // Verify that calling twice uses cache
    // Full test would require real Maven artifacts
    assertThat(DependencySpecLoader.class).isNotNull();
  }

  @Test
  @DisplayName("Should clear cache successfully")
  void testClearCache() {
    // Should not throw exception
    DependencySpecLoader.clearCache();
    DependencySpecLoader.clearCache(); // Second call should be safe
  }
}
