package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.math.BigDecimal;
import java.util.Objects;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class MinBigDecimalValidator implements ConstraintValidator<MinBigDecimal, BigDecimal> {

    private BigDecimal minimum;
    private boolean exclusive;

    @Override
    public void initialize(MinBigDecimal constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.minimum = new BigDecimal(constraintAnnotation.minimum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.compareTo(this.minimum) > 0 || (!exclusive && value.compareTo(this.minimum) == 0));
    }
}