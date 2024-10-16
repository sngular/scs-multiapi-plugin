package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.util.Objects;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class MaxDoubleValidator implements ConstraintValidator<MaxDouble, Double> {

    private double maximum;
    private boolean exclusive;

    @Override
    public void initialize(MaxDouble constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.maximum = Double.parseDouble(constraintAnnotation.maximum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Double value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.doubleValue() < this.maximum || (!exclusive && value.doubleValue() == this.maximum));
    }
}