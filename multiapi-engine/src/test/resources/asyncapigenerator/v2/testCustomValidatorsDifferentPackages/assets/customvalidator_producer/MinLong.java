package com.sngular.scsplugin.customvalidatordiff.model.event.producer.customvalidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MinLongValidator.class)
@Documented
public @interface MinLong {
    String minimum();
    boolean exclusive();
    String message() default "Value is smaller than the minimum.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
