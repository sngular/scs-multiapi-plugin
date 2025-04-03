package com.sngular.scsplugin.filegeneration.model.event.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.util.Objects;

public class NotNullValidator implements ConstraintValidator<NotNull, Object> {

    @Override
    public void initialize(NotNull constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
    }

    @Override
    public boolean isValid(Object value, ConstraintValidatorContext context) {
        return Objects.nonNull(value);
    }
}