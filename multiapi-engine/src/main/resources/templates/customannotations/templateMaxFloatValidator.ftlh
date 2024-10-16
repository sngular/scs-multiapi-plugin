package ${packageModel}.customvalidator;

import java.util.Objects;

import ${javaEEPackage}.validation.ConstraintValidator;
import ${javaEEPackage}.validation.ConstraintValidatorContext;

public class MaxFloatValidator implements ConstraintValidator<MaxFloat, Float> {

    private float maximum;
    private boolean exclusive;

    @Override
    public void initialize(MaxFloat constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
        this.maximum = Float.parseFloat(constraintAnnotation.maximum());
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
    public boolean isValid(Float value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.floatValue() < this.maximum || (!exclusive && value.floatValue() == this.maximum));
    }
}