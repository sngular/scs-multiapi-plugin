package ${packageModel}.customvalidator;

import ${javaEEPackage}.validation.Constraint;
import ${javaEEPackage}.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MinBigDecimalValidator.class)
@Documented
public @interface MinBigDecimal {
    String minimum();
    boolean exclusive();
    String message() default "Value is smaller than the minimum.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}