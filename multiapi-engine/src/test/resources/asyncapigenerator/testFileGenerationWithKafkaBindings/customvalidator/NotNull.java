package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.schemas.customvalidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER,
    ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = NotNullValidator.class)
@Documented
public @interface NotNull {

    String message() default "Value is null.";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}