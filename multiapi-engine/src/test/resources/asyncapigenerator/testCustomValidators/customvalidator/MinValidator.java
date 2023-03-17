package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinValidator implements ConstraintValidator<Min, Integer> {

    private int minimum;
    private boolean exclusive;

    @Override
    public void initialize(Min constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.minimum = constraintAnnotation.minimum();
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        if (exclusive) {
            if (value <= this.minimum) {
                return false;
            } else {
                return true;
            }
        }
        if (value < this.minimum) {
            return false;
        } else {
            return true;
        }
    }
}