package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.util.List;
import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MaxItemsValidator implements ConstraintValidator<MaxItems, List<?>> {

  private int maximum;

  @Override
  public void initialize(MaxItems constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.maximum = constraintAnnotation.maximum();
  }

  @Override
  public boolean isValid(List<?> value, ConstraintValidatorContext context) {
    if (Objects.isNull(value))
      return true;

    if (value.size() > this.maximum) {
      return false;
    } else {
      return true;
    }
  }
}