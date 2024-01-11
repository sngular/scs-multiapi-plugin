package com.sngular.api.generator.plugin.common.util;

import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.common.files.ClasspathFileLocation;
import com.sngular.api.generator.plugin.common.files.DirectoryFileLocation;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Objects;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

public class GeneratorUtil {

  public static Pair<InputStream, FileLocation> resolveYmlLocation(
      final String ymlFilePath, Class<?> GeneratorClass) {
    final InputStream ymlFile;
    final FileLocation ymlParentPath;
    final InputStream classPathInput;
    try {
      classPathInput = GeneratorClass.getClassLoader().getResourceAsStream(ymlFilePath);
      if (Objects.nonNull(classPathInput)) {
        ymlFile = classPathInput;
        ymlParentPath = new ClasspathFileLocation(ymlFilePath);
      } else {
        final File f = new File(ymlFilePath);
        ymlFile = new FileInputStream(f);
        ymlParentPath = new DirectoryFileLocation(f.toPath().getParent());
      }
    } catch (final IOException e) {
      throw new FileSystemException(e.getMessage());
    }
    return new ImmutablePair<>(ymlFile, ymlParentPath);
  }
}
