package com.sngular.multifileplugin.testapi.model.customvalidator;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MultipleOfValidator.class)
@Documented
public @interface MultipleOf {

  String multiple();

  String message() default "Value must be a multiple of the number.";

  Class<?>[] groups() default {};

  Class<? extends Payload>[] payload() default {};

}