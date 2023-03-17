package com.sngular.multifileplugin.testapi.model.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MaxValidator implements ConstraintValidator<Max, Integer> {

    private int maximum;
    private boolean exclusive;

    @Override
    public void initialize(Max constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.maximum = constraintAnnotation.maximum();
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        if (exclusive) {
            if (value >= this.maximum) {
                return false;
            } else {
                return true;
            }
        }
        if (value > this.maximum) {
            return false;
        } else {
            return true;
        }
    }
}