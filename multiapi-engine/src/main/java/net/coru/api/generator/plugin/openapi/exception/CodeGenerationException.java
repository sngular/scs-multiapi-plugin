package net.coru.api.generator.plugin.openapi.exception;

public class CodeGenerationException extends RuntimeException {

  public CodeGenerationException(final String message, final Throwable cause) {
    super(message, cause);
  }
}
