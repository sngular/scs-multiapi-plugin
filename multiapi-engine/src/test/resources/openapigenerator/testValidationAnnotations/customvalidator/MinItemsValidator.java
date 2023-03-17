package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.util.List;
import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinItemsValidator implements ConstraintValidator<MinItems, List<?>> {

  private int minimum;

  @Override
  public void initialize(MinItems constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.minimum = constraintAnnotation.minimum();
  }

  @Override
  public boolean isValid(List<?> value, ConstraintValidatorContext context) {
    if (Objects.isNull(value))
      return true;

    if (value.size() < this.minimum) {
      return false;
    } else {
      return true;
    }
  }
}