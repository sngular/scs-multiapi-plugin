package com.sngular.scsplugin.customvalidatordiff.model.event.consumer.customvalidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MaxLongValidator.class)
@Documented
public @interface MaxLong {
    String maximum();
    boolean exclusive();
    String message() default "Value is bigger than the maximum.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
