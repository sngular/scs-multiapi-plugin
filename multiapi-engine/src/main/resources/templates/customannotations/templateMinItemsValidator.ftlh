package ${packageModel}.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MinItemsValidator implements ConstraintValidator<MinItems, Array> {

  private int minimum;

  @Override
  public void initialize(MinItems constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.minimum = constraintAnnotation.minimum()
  }

  @Override
  public boolean isValid(Array value, ConstraintValidatorContext context) {
    if (value.size() < this.minimum) {
      return false;
    } else {
      return true;
    }
  }
}