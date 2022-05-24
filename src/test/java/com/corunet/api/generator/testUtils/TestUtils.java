package com.corunet.api.generator.testUtils;

import static junit.framework.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

import org.apache.commons.io.IOUtils;

public class TestUtils {

  public static void validateFiles(
      final List<File> expectedConsumerFiles, final List<File> expectedProducerFiles, final File targetConsumerDirectory, final File targetProducerDirectory)
      throws IOException {
    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
  }

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
}
