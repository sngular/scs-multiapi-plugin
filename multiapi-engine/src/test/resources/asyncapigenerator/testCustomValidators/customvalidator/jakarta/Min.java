package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER,
    ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MinValidator.class)
@Documented
public @interface Min {
    String minimum();
    boolean exclusive();
    String message() default "Value is smaller than the minimum.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}