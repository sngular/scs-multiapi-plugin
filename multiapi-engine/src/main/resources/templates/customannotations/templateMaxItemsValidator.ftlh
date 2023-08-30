package ${packageModel}.${schema.parentPackage}.customvalidator;

import java.util.List;
import java.util.Objects;

import ${javaEEPackage}.validation.ConstraintValidator;
import ${javaEEPackage}.validation.ConstraintValidatorContext;

public class MaxItemsValidator implements ConstraintValidator<MaxItems, List<?>> {

  private int maximum;

  @Override
  public void initialize(MaxItems constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.maximum = constraintAnnotation.maximum();
  }

  @Override
  public boolean isValid(List<?> value, ConstraintValidatorContext context) {
    return Objects.isNull(value) || !(value.size() > this.maximum);
  }
}