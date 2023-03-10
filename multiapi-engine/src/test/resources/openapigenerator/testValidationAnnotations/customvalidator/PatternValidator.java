package com.sngular.multifileplugin.testapi.model.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class PatternValidator implements ConstraintValidator<Pattern, String> {

  private String regex;

  @Override
  public void initialize(Pattern constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.regex = constraintAnnotation.regex();
  }

  @Override
  public boolean isValid(String value, ConstraintValidatorContext constraintValidatorContext) {
    return value.matches(regex);
  }
}