package ${packageModel}.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinValidator implements ConstraintValidator<Min, Integer> {

    private Integer minimum;

    @Override
    public void initialize(Min constraintAnnotation) {
        this.minimum = constraintAnnotation.minimum()
    }

    @Override
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        if (value < this.minimum) {
            return false;
        } else {
            return true;
        }
    }
}