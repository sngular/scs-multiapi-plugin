package ${packageModel}.customvalidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER,
    ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = MaxItemsValidator.class)
@Documented
public @interface MaxItems {
  int maximum();
  String message() default "Array has more items than allowed.";
  Class<?>[] groups() default {};
  Class<? extends Payload>[] payload() default {};
}