package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.util.List;
import java.util.Objects;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class MaxItemsValidator implements ConstraintValidator<MaxItems, List<?>> {

  private int maximum;

  @Override
  public void initialize(MaxItems constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.maximum = constraintAnnotation.maximum();
  }

  @Override
  public boolean isValid(List<?> value, ConstraintValidatorContext context) {
    return Objects.isNull(value) || !(value.size() > this.maximum);
  }
}