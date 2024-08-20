package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.math.BigDecimal;
import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

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
        return Objects.isNull(value) || (value < this.maximum || (!exclusive && value == this.maximum));
    }
}