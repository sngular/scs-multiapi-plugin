package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinValidator implements ConstraintValidator<MinFloat, Float> {

    private float minimum;
    private boolean exclusive;

    @Override
    public void initialize(MinFloat constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.minimum = Float.parseFloat(constraintAnnotation.minimum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Float value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.floatValue() > this.minimum || (!exclusive && value.floatValue() == this.minimum));
    }
}