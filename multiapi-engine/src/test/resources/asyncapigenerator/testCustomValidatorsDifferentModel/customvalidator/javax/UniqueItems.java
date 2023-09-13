package com.sngular.scsplugin.customvalidator.event.customvalidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER,
    ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = UniqueItemsValidator.class)
@Documented
public @interface UniqueItems {
  String message() default "Items are not unique.";
  Class<?>[] groups() default {};
  Class<? extends Payload>[] payload() default {};

}