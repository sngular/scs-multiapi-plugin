package ${packageModel}.customvalidator;

import ${javaEEPackage}.validation.Constraint;
import ${javaEEPackage}.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER,
ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MaxBigDecimalValidator.class)
@Documented
public @interface MaxBigDecimal {
    String maximum();
    boolean exclusive();
    String message() default "Value is bigger than the maximum.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}