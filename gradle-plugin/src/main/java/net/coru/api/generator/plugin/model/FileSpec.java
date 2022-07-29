package net.coru.api.generator.plugin.model;

import org.gradle.api.tasks.Nested;

public abstract class FileSpec {

  private String filePath;

  @Nested
  abstract public OperationParameterObject supplier();

  @Nested
  abstract public OperationParameterObject consumer();

  @Nested
  abstract public OperationParameterObject streamBridge();

}