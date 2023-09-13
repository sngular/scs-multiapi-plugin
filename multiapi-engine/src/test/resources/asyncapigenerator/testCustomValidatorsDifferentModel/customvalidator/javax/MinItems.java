package com.sngular.scsplugin.customvalidatordifferentmodel.event.producer.model.customvalidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER,
    ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MinItemsValidator.class)
@Documented
public @interface MinItems {
  int minimum();
  String message() default "Array has fewer items than allowed.";
  Class<?>[] groups() default {};
  Class<? extends Payload>[] payload() default {};
}