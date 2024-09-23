package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MaxIntegerValidator implements ConstraintValidator<MaxInteger, Integer> {

    private int maximum;
    private boolean exclusive;

    @Override
    public void initialize(MaxInteger constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.maximum = Integer.parseInt(constraintAnnotation.maximum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.intValue() < this.maximum || (!exclusive && value.intValue() == this.maximum));
    }
}