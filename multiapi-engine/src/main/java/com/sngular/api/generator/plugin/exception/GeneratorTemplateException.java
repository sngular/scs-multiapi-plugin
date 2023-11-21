package com.sngular.api.generator.plugin.exception;

public class GeneratorTemplateException extends RuntimeException {

  private static final String ERROR_MESSAGE = "A Template Factory error has been occurred: %s";

  private static final String ERROR_MESSAGE_FOR_FILE = "A Template Factory error has been occurred working with the file %s error description: %s";

  public GeneratorTemplateException(final String message, final Throwable cause) {
    super(String.format(ERROR_MESSAGE, message), cause);
  }

  public GeneratorTemplateException(final String message, final String filePath, final Throwable cause) {
    super(String.format(ERROR_MESSAGE_FOR_FILE, filePath, message), cause);
  }
}
