package ${packageModel}.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class SizeValidator implements ConstraintValidator<Size, Integer> {

    private int min;
    private int max;

    @Override
    public void initialize(Size constraintAnnotation) {
        this.min = constraintAnnotation.min();
        this.max = constraintAnnotation.max();
    }

    @Override
    public boolean isValid(Integer value, ConstraintValidatorContext context) {

        if (max != 0 && min == 0){
            return value <= max;
        } else if (min != 0 && max == 0){
            return value >= min;
        } else if (max != 0 && min != 0){
            return value <= max && value >= min;
        }

        return false;

    }
}