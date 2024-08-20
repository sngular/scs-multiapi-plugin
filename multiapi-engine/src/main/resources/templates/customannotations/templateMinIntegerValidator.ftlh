package ${packageModel}.customvalidator;

import java.util.Objects;

import ${javaEEPackage}.validation.ConstraintValidator;
import ${javaEEPackage}.validation.ConstraintValidatorContext;

public class MinValidator implements ConstraintValidator<MinInteger, Integer> {

    private int minimum;
    private boolean exclusive;

    @Override
    public void initialize(MinInteger constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.minimum = Integer.parseInt(constraintAnnotation.minimum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.intValue() > this.minimum || (!exclusive && value.intValue() == this.minimum));
    }
}