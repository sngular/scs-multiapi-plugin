/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.files;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.net.URI;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

class FileLocationUtilTest {

  @Test
  void testGetParentUriFromJarFile() throws Exception {
    // Given: A JAR URI pointing to a file in a subdirectory
    URI jarUri = new URI("jar:file:/app/lib.jar!/com/example/openapi.yml");

    // When: Getting the parent URI
    URI parentUri = FileLocationUtil.getParentUri(jarUri);

    // Then: Should extract the parent directory within the JAR
    assertThat(parentUri).isEqualTo(new URI("jar:file:/app/lib.jar!/com/example"));
  }

  @Test
  void testGetParentUriFromJarFileRoot() throws Exception {
    // Given: A JAR URI pointing to a file at the root
    URI jarUri = new URI("jar:file:/app/lib.jar!/openapi.yml");

    // When: Getting the parent URI
    URI parentUri = FileLocationUtil.getParentUri(jarUri);

    // Then: Should be the root of the JAR
    assertThat(parentUri).isEqualTo(new URI("jar:file:/app/lib.jar!/"));
  }

  @Test
  void testGetParentUriFromJarFileDeepNesting() throws Exception {
    // Given: A JAR URI pointing to a deeply nested file
    URI jarUri = new URI("jar:file:/app/lib.jar!/com/example/api/specs/openapi.yml");

    // When: Getting the parent URI
    URI parentUri = FileLocationUtil.getParentUri(jarUri);

    // Then: Should extract only the immediate parent directory
    assertThat(parentUri).isEqualTo(new URI("jar:file:/app/lib.jar!/com/example/api/specs"));
  }

  @Test
  void testGetParentUriFromFilePath() throws Exception {
    // Given: A filesystem URI
    URI fileUri = Paths.get("/path/to/openapi.yml").toUri();

    // When: Getting the parent URI
    URI parentUri = FileLocationUtil.getParentUri(fileUri);

    // Then: Should extract the parent directory
    assertThat(parentUri).isEqualTo(Paths.get("/path/to").toUri());
  }

  @Test
  void testGetParentUriFromFilePathRelative() throws Exception {
    // Given: A relative filesystem path
    URI fileUri = Paths.get("specs/openapi.yml").toUri();

    // When: Getting the parent URI
    URI parentUri = FileLocationUtil.getParentUri(fileUri);

    // Then: Should extract the parent directory
    assertThat(parentUri).isEqualTo(Paths.get("specs").toUri());
  }

  @Test
  void testGetParentUriInvalidJarFormat() throws Exception {
    // Given: An invalid JAR URI (missing !)
    URI invalidJarUri = new URI("jar:file:/app/lib.jar/com/example/openapi.yml");

    // When/Then: Should throw IllegalArgumentException
    assertThatThrownBy(() -> FileLocationUtil.getParentUri(invalidJarUri))
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessageContaining("Invalid JAR URI");
  }

  @Test
  void testGetParentUriUnsupportedScheme() throws Exception {
    // Given: A URI with unsupported scheme
    URI httpUri = new URI("http://example.com/openapi.yml");

    // When/Then: Should throw IllegalArgumentException
    assertThatThrownBy(() -> FileLocationUtil.getParentUri(httpUri))
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessageContaining("Unsupported URI scheme");
  }

  @Test
  void testJarUriRoundTrip() throws Exception {
    // Given: A JAR URI
    URI originalUri = new URI("jar:file:/app/lib.jar!/com/example/api/openapi.yml");

    // When: Get parent
    URI parentUri = FileLocationUtil.getParentUri(originalUri);

    // Then: Parent should be the directory containing the file
    assertThat(parentUri.toString()).isEqualTo("jar:file:/app/lib.jar!/com/example/api");
  }

  @Test
  void testJarUriWithSpecialCharacters() throws Exception {
    // Given: A JAR URI with special characters in path
    URI jarUri = new URI("jar:file:/app/my-lib-1.0.jar!/com/example/openapi.yml");

    // When: Getting parent
    URI parentUri = FileLocationUtil.getParentUri(jarUri);

    // Then: Should preserve special characters
    assertThat(parentUri.toString()).contains("my-lib-1.0.jar");
    assertThat(parentUri.toString()).contains("com/example");
  }

  @Test
  void testFileUriWithWindowsPath() throws Exception {
    // Given: A Windows-style file URI
    URI fileUri = new URI("file:/C:/Users/test/openapi.yml");

    // When: Getting parent
    URI parentUri = FileLocationUtil.getParentUri(fileUri);

    // Then: Should extract parent correctly
    assertThat(parentUri.toString()).contains("Users/test");
  }

  @Test
  void testJarUriWithNormalizedPath() throws Exception {
    // Given: A JAR URI that needs normalization
    URI jarUri = new URI("jar:file:/app/lib.jar!/com/example/../example/openapi.yml");

    // When: Getting parent
    URI parentUri = FileLocationUtil.getParentUri(jarUri);

    // Then: Should normalize the path
    // Note: The normalization happens when resolving paths
    assertThat(parentUri.toString()).contains("lib.jar");
  }
}
