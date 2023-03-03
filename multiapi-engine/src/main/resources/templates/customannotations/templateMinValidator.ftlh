package ${packageModel};

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinValidator implements ConstraintValidator<Min, Integer> {
@Override
public void initialize(Min constraintAnnotation) {
}

@Override
public boolean isValid(Integer value, ConstraintValidatorContext context) {
    if (value < context) {
        return false;
    } else {
        return true;
    }
}
}