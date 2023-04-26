package com.sngular.api.generator.plugin.exception;

public class GeneratorTemplateException extends RuntimeException {

  private static final String ERROR_MESSAGE = "A Template Factory error has been occurred: %s";

  public GeneratorTemplateException(final String message, final Throwable cause) {
    super(String.format(ERROR_MESSAGE, message), cause);
  }
}
