/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.test.utils;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public class TestUtils {

  public static void validateFiles(final List<String> expectedFiles, final File targetDirectory) throws URISyntaxException {
    File reader1;
    File reader2;

    List<File> outputFiles = new ArrayList<>(List.of(Objects.requireNonNull(targetDirectory.listFiles())));
    outputFiles.removeIf(File::isDirectory);
    outputFiles.sort(Comparator.comparing(File::getPath));
    assertThat(outputFiles)
        .overridingErrorMessage("Wrong Number of files %d vs %d: %s", outputFiles.size(), expectedFiles.size(), outputFiles)
        .hasSize(expectedFiles.size());
    for (int i = 0; i < outputFiles.size(); i++) {
      reader1 = outputFiles.get(i);
      assertThat(reader1).overridingErrorMessage("Generated file %s should not be null", outputFiles.get(i)).isNotNull();
      final String sourceName = expectedFiles.get(i);
      reader2 = TestUtils.resourceAsFile(sourceName);
      assertThat(reader2).overridingErrorMessage("Expected file %s should not be null", sourceName).isNotNull();
      assertThat(reader1)
          .hasSameTextualContentAs(reader2);
    }
  }

  public static File resourceAsFile(String resourceName) throws URISyntaxException {
    return Paths.get(TestUtils.class.getClassLoader().getResource(resourceName).toURI()).toFile();
  }
}
