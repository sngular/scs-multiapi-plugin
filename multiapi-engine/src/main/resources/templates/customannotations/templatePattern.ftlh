package ${packageModel}.customvalidator;

import ${javaEEPackage}.validation.Constraint;
import ${javaEEPackage}.validation.Payload;

import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = PatternValidator.class)
@Documented
public @interface Pattern {

  String regex();

  String message() default "Value does not fulfill the pattern.";

  Class<?>[] groups() default {};

  Class<? extends Payload>[] payload() default {};

}