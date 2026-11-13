package com.sngular.scsplugin.streetlights.model.event.customvalidator;

import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

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
