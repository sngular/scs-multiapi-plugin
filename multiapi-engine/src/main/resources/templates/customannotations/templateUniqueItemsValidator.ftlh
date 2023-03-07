package ${packageModel}.customvalidator;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class SizeValidator implements ConstraintValidator<UniqueItems, Integer> {

  @Override
  public void initialize(UniqueItems constraintAnnotation) {}

  @Override
  public boolean isValid(Array value, ConstraintValidatorContext context) {
    Set<?> s = new HashSet<?>(Arrays.asList(value));
    if (s.size() == value.size()){
      return true;
    } else {
    return false;
    }
  }
}