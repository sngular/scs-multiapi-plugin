package net.coru.api.generator.plugin.openapi.exception;

public class FileParseException extends RuntimeException {
  private final static String MESSAGE = "Code generation failed";

  public FileParseException(final String message) {
    super(MESSAGE + " " + message);
  }
}
