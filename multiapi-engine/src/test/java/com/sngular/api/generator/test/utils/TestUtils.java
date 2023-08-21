/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.test.utils;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public class TestUtils {

  public static void validateFiles(final List<String> expectedFiles, final File targetDirectory) throws IOException {
    InputStream reader1;
    InputStream reader2;

    List<File> outputFiles = new ArrayList<>(List.of(Objects.requireNonNull(targetDirectory.listFiles())));
    outputFiles.removeIf(File::isDirectory);
    outputFiles.sort(Comparator.comparing(File::getPath));
    assertThat(outputFiles)
            .overridingErrorMessage("Wrong Number of files %d vs %d: %s", outputFiles.size(), expectedFiles.size(), outputFiles)
            .hasSize(expectedFiles.size());
    for (int i = 0; i < outputFiles.size(); i++) {
      reader1 = new FileInputStream(outputFiles.get(i));
      assertThat(reader1).overridingErrorMessage("Output File : %s not found", outputFiles.get(i)).isNotNull();
      final String sourceName = expectedFiles.get(i);
      reader2 = TestUtils.resourceAsFile(sourceName);
      assertThat(reader2).overridingErrorMessage("Expected File : %s not found", sourceName).isNotNull();
      int finalI = i;
      assertThat(reader1).as(() -> "Unexpected content for file " + outputFiles.get(finalI)).hasSameContentAs(reader2);
    }
  }

  public static InputStream resourceAsFile(String resourceName) {
    return TestUtils.class.getClassLoader().getResourceAsStream(resourceName);
  }
}
