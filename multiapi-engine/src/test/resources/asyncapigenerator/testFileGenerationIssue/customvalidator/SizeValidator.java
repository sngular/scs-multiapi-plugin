package com.sngular.scsplugin.filegenerationissue.model.event.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class SizeValidator implements ConstraintValidator<Size, String> {

    private int min;
    private int max;

    @Override
    public void initialize(Size constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.min = constraintAnnotation.min();
        this.max = constraintAnnotation.max();
    }

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {

        if (max != 0 && min == 0){
            return value.length() <= max;
        } else if (min != 0 && max == 0){
            return value.length() >= min;
        } else if (max != 0 && min != 0){
            return value.length() <= max && value.length() >= min;
        }

        return false;

    }
}