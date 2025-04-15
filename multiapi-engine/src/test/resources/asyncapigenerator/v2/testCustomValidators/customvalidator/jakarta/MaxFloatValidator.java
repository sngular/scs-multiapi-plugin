package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.util.Objects;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class MaxFloatValidator implements ConstraintValidator<MaxFloat, Float> {

    private float maximum;
    private boolean exclusive;

    @Override
    public void initialize(MaxFloat constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.maximum = Float.parseFloat(constraintAnnotation.maximum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Float value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.floatValue() < this.maximum || (!exclusive && value.floatValue() == this.maximum));
    }
}