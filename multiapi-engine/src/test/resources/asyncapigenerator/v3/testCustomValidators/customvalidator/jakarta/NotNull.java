package com.sngular.scsplugin.customvalidator.model.event.customvalidator;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = NotNullValidator.class)
@Documented
public @interface NotNull {

    String message() default "Value is null.";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}