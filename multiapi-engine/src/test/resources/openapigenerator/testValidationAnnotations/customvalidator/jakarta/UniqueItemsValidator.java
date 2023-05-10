package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class UniqueItemsValidator implements ConstraintValidator<UniqueItems, List<?>> {

  @Override
  public void initialize(UniqueItems constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
  }

  @Override
  public boolean isValid(List<?> value, ConstraintValidatorContext context) {
    if (Objects.isNull(value))
      return true;

    Set<?> s = new HashSet<>(value);
    return s.size() == value.size();
  }
}