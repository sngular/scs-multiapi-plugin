package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class UniqueItemsValidator implements ConstraintValidator<UniqueItems, List> {

  @Override
  public void initialize(UniqueItems constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
  }

  @Override
  public boolean isValid(List value, ConstraintValidatorContext context) {
    Set<?> s = new HashSet<>(Arrays.asList(value));
    if (s.size() == value.size()){
      return true;
    } else {
      return false;
    }
  }
}