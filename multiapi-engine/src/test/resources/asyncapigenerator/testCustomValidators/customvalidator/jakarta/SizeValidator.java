package com.sngular.scsplugin.customvalidator.model.event.schemas.customvalidator;

import java.util.Objects;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

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
        return Objects.isNull(value) || ((max == 0 || value.length() <= max) && (min == 0 || value.length() >= min));
    }
}