package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.util.Objects;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class MaxLongValidator implements ConstraintValidator<MaxLong, Long> {

    private long maximum;
    private boolean exclusive;

    @Override
    public void initialize(MaxLong constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.maximum = Long.parseLong(constraintAnnotation.maximum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Long value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.longValue() < this.maximum || (!exclusive && value.longValue() == this.maximum));
    }
}
