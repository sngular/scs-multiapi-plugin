package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import java.math.BigDecimal;
import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinValidator implements ConstraintValidator<MinBigDecimal, BigDecimal> {

    private BigDecimal minimum;
    private boolean exclusive;

    @Override
    public void initialize(Min constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.minimum = new BigDecimal(constraintAnnotation.minimum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value > this.minimum || (!exclusive && value == this.minimum));
    }
}