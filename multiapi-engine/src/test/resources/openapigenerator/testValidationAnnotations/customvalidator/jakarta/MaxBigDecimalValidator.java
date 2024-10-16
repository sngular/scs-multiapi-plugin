package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.math.BigDecimal;
import java.util.Objects;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class MaxBigDecimalValidator implements ConstraintValidator<MaxBigDecimal, BigDecimal> {

    private BigDecimal maximum;
    private boolean exclusive;

    @Override
    public void initialize(MaxBigDecimal constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.maximum = new BigDecimal(constraintAnnotation.maximum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.compareTo(this.maximum) < 0 || (!exclusive && value.compareTo(this.maximum) == 0);
    }
}