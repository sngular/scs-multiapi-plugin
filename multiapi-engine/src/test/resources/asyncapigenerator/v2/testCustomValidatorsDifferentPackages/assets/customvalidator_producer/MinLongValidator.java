package com.sngular.scsplugin.customvalidatordiff.model.event.producer.customvalidator;

import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinLongValidator implements ConstraintValidator<MinLong, Long> {

    private long minimum;
    private boolean exclusive;

    @Override
    public void initialize(MinLong constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.minimum = Long.parseLong(constraintAnnotation.minimum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Long value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.longValue() > this.minimum || (!exclusive && value.longValue() == this.minimum));
    }
}
