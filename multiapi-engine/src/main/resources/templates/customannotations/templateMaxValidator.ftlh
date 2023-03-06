package ${packageModel}.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MaxValidator implements ConstraintValidator<Max, Integer> {

    private Integer maximum;

    @Override
    public void initialize(Max constraintAnnotation) {
        this.maximum = constraintAnnotation.maximum()
    }

    @Override
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        if (value > this.maximum) {
            return false;
        } else {
            return true;
        }
    }
}