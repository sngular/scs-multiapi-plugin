/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.test.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

import static org.assertj.core.api.Assertions.assertThat;

public class TestUtils {

  public static void validateFiles(final List<String> expectedFiles, final File targetDirectory) throws IOException {
    InputStream reader1;
    InputStream reader2;

    List<File> outputFiles = new ArrayList<>(List.of(Objects.requireNonNull(targetDirectory.listFiles())));
    outputFiles.removeIf(File::isDirectory);
    outputFiles.sort(Comparator.comparing(File::getPath));
    assertThat(outputFiles).hasSize(expectedFiles.size());
    for (int i = 0; i < outputFiles.size(); i++) {
      final File outputFile = outputFiles.get(i);
      reader1 = new FileInputStream(outputFile);
      final String sourceName = expectedFiles.get(i);
      reader2 = TestUtils.resourceAsFile(sourceName);
      assertThat(reader2).as(() -> "Unexpected content for file " + outputFile.getAbsolutePath()).hasSameContentAs(reader1);
    }
  }

  public static void checkTargetFiles(final List<String> expectedFileNames, final File targetDirectory) {
    assertThat(targetDirectory).isNotEmptyDirectory();
    assertThat(targetDirectory.list()).containsAll(expectedFileNames);
  }

  public static InputStream resourceAsFile(String resourceName) {
    return TestUtils.class.getClassLoader().getResourceAsStream(resourceName);
  }
}
