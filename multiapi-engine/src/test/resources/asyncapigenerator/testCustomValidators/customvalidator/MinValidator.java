package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinValidator implements ConstraintValidator<Min, Integer> {

    private int minimum;
    private boolean exclusive;

    @Override
    public void initialize(Min constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.minimum = Integer.parseInt(constraintAnnotation.minimum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.intValue() > this.minimum || (!exclusive && value.intValue() == this.minimum));
    }
}