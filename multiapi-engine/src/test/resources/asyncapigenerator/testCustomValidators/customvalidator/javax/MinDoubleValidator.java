package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinDoubleValidator implements ConstraintValidator<MinDouble, Double> {

    private double minimum;
    private boolean exclusive;

    @Override
    public void initialize(MinDouble constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.minimum = Double.parseDouble(constraintAnnotation.minimum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Double value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.doubleValue() > this.minimum || (!exclusive && value.doubleValue() == this.minimum));
    }
}