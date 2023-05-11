package com.sngular.api.generator.plugin.asyncapi.exception;

public class InvalidAPIException extends RuntimeException {

  private static final String ERROR_MESSAGE = "AsyncApi -> There is at least one operation without operationId";

  public InvalidAPIException() {
    super(ERROR_MESSAGE);
  }

}
