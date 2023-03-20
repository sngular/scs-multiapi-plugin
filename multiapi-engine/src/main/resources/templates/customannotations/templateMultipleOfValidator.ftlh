package ${packageModel}.customvalidator;

import java.math.BigDecimal;
import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class MultipleOfValidator implements ConstraintValidator<MultipleOf, BigDecimal> {

  private String multiple;

  @Override
  public void initialize(final MultipleOf constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
    this.multiple = new BigDecimal(constraintAnnotation.multiple());
  }

  @Override
  public boolean isValid(final BigDecimal bigDecimal, final ConstraintValidatorContext constraintValidatorContext) {
    BigDecimal[] divisionResult = bigDecimal.divideAndRemainder(multiple);

    return Objects.equals(divisionResult[1], new BigDecimal(0));
  }
}