package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MaxIntegerValidator.class)
@Documented
public @interface MaxInteger {
    String maximum();
    boolean exclusive();
    String message() default "Value is bigger than the maximum.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}