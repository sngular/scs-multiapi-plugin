package ${packageModel};

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinValidator implements ConstraintValidator<Max, Integer> {
@Override
public void initialize(Max constraintAnnotation) {
}

@Override
public boolean isValid(Integer value, ConstraintValidatorContext context) {
    if (value > context) {
        return false;
    } else {
        return true;
    }
}
}