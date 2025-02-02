package com.sngular.multifileplugin.testapi.model.customvalidator;

import java.math.BigDecimal;
import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

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