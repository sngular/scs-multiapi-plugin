/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.test.utils;

import static junit.framework.Assert.assertTrue;
import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

import org.apache.commons.io.IOUtils;

public class TestUtils {

  public static void validateFiles(final List<File> expectedFiles, final File targetDirectory) throws IOException {
    FileInputStream reader1;
    FileInputStream reader2;

    List<File> outputFiles = new ArrayList<>(List.of(Objects.requireNonNull(targetDirectory.listFiles())));
    outputFiles.removeIf(File::isDirectory);
    outputFiles.sort(Comparator.comparing(File::getPath));

    for (int i = 0; i < outputFiles.size(); i++) {
      reader1 = new FileInputStream(outputFiles.get(i));
      reader2 = new FileInputStream(expectedFiles.get(i));
      assertTrue(IOUtils.contentEquals(reader1, reader2));
    }
  }

  public static void checkTargetFiles(final List<String> expectedFileNames, final File targetDirectory) {
    assertThat(targetDirectory).isNotEmptyDirectory();
    assertThat(targetDirectory.list()).containsAll(expectedFileNames);
  }
}
