package net.coru.multifileplugin.parameterwithcontent.model.exception;

public class ModelClassException extends RuntimeException {

  private static final String ERROR_MESSAGE = "There are some problems related to the entity called %s. Maybe could be caused by required fields or anyOf/oneOf restrictions";

  public ModelClassException(final String modelEntity) {
    super(String.format(ERROR_MESSAGE, modelEntity));
  }
}