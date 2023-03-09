package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MaxItemsValidator implements ConstraintValidator<MaxItems, Array> {

  private int maximum;

  @Override
  public void initialize(MaxItems constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.maximum = constraintAnnotation.maximum()
  }

  @Override
  public boolean isValid(Array value, ConstraintValidatorContext context) {
    if (value.size() > this.maximum) {
      return false;
    } else {
      return true;
    }
  }
}