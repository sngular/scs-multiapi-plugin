package ${packageModel}.customvalidator;

import ${javaEEPackage}.validation.Constraint;
import ${javaEEPackage}.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER,
    ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MinDoubleValidator.class)
@Documented
public @interface MinDouble {
    String minimum();
    boolean exclusive();
    String message() default "Value is smaller than the minimum.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}