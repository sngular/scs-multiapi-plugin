package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.util.Objects;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class MultipleOfValidator implements ConstraintValidator<MultipleOf, Integer> {

  private int multiple;

  @Override
  public void initialize(final MultipleOf constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.multiple = Integer.parseInt(constraintAnnotation.multiple());
  }

  @Override
  public boolean isValid(final Integer value, final ConstraintValidatorContext constraintValidatorContext) {
    return value.intValue() % multiple == 0;
  }
}