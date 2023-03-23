package ${packageModel}.customvalidator;

<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && field.dataTypeSimple == "BigDecimal">
import java.math.BigDecimal;
    </#if>
</#list>
import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigDecimal")>
public class MultipleOfValidator implements ConstraintValidator<MultipleOf, BigDecimal> {

  private BigDecimal multiple;
      <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
public class MultipleOfValidator implements ConstraintValidator<MultipleOf, Integer> {

  private int multiple;
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
public class MultipleOfValidator implements ConstraintValidator<MultipleOf, Float> {

  private float multiple;
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
public class MultipleOfValidator implements ConstraintValidator<MultipleOf, Double> {

  private double multiple;
    </#if>
</#list>

  @Override
  public void initialize(final MultipleOf constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigDecimal")>
    this.multiple = new BigDecimal(constraintAnnotation.multiple());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
    this.multiple = Integer.parseInt(constraintAnnotation.multiple());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
    this.multiple = Float.parseFloat(constraintAnnotation.multiple());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
    this.multiple = Double.parseDouble(constraintAnnotation.multiple());
        <#break>
    </#if>
</#list>
  }

  @Override
<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigDecimal")>
  public boolean isValid(final BigDecimal value, final ConstraintValidatorContext constraintValidatorContext) {
    BigDecimal[] divisionResult = value.divideAndRemainder(multiple);

    return Objects.equals(divisionResult[1], new BigDecimal(0));
  }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
  public boolean isValid(final Integer value, final ConstraintValidatorContext constraintValidatorContext) {
    return value.intValue() % multiple == 0;
  }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
  public boolean isValid(final Float value, final ConstraintValidatorContext constraintValidatorContext) {
    return value.floatValue() % multiple == 0;
  }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
  public boolean isValid(final Double value, final ConstraintValidatorContext constraintValidatorContext) {
    return value.doubleValue() % multiple == 0;
  }
        <#break>
    </#if>
</#list>
}

