package com.sngular.scsplugin.filegenerationissue.model.event.schemas.customvalidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER,
    ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = SizeValidator.class)
@Documented
public @interface Size {

    int min();

    int max();

    String message() default "Value is not between the correct values.";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

}