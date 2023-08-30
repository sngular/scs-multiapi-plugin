package com.sngular.scsplugin.customvalidator.model.event.schemas.customvalidator;

import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

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