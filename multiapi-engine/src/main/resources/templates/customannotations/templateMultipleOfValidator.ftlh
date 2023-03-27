package ${packageModel}.customvalidator;

<#list schema.fieldObjectList as field>
  <#if field.dataType.baseType?has_content && field.dataType.baseType == "BigDecimal">
import java.math.BigDecimal;
  </#if>
</#list>
import java.util.Objects;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

<#list schema.fieldObjectList as field>
  <#if field.dataType.baseType?has_content && (field.dataType.baseType == "BigDecimal" || field.dataType.baseType == "bigDecimal")>
public class MultipleOfValidator implements ConstraintValidator<MultipleOf, BigDecimal> {

  private BigDecimal multiple;
      <#break>
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
public class MultipleOfValidator implements ConstraintValidator<MultipleOf, Integer> {

  private int multiple;
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
public class MultipleOfValidator implements ConstraintValidator<MultipleOf, Float> {

  private float multiple;
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
public class MultipleOfValidator implements ConstraintValidator<MultipleOf, Double> {

  private double multiple;
  </#if>
</#list>

  @Override
  public void initialize(final MultipleOf constraintAnnotation) {
    ConstraintValidator.super.initialize(constraintAnnotation);
<#list schema.fieldObjectList as field>
  <#if field.dataType.baseType?has_content && (field.dataType.baseType == "BigDecimal" || field.dataType.baseType == "bigDecimal")>
    this.multiple = new BigDecimal(constraintAnnotation.multiple());
      <#break>
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
    this.multiple = Integer.parseInt(constraintAnnotation.multiple());
      <#break>
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
    this.multiple = Float.parseFloat(constraintAnnotation.multiple());
      <#break>
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
    this.multiple = Double.parseDouble(constraintAnnotation.multiple());
      <#break>
  </#if>
</#list>
  }

  @Override
<#list schema.fieldObjectList as field>
  <#if field.dataType.baseType?has_content && (field.dataType.baseType == "BigDecimal" || field.dataType.baseType == "bigDecimal")>
  public boolean isValid(final BigDecimal value, final ConstraintValidatorContext constraintValidatorContext) {
    BigDecimal[] divisionResult = value.divideAndRemainder(multiple);

    return Objects.equals(divisionResult[1], new BigDecimal(0));
  }
      <#break>
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
  public boolean isValid(final Integer value, final ConstraintValidatorContext constraintValidatorContext) {
    return value.intValue() % multiple == 0;
  }
    <#break>
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
  public boolean isValid(final Float value, final ConstraintValidatorContext constraintValidatorContext) {
    return value.floatValue() % multiple == 0;
  }
    <#break>
  <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
  public boolean isValid(final Double value, final ConstraintValidatorContext constraintValidatorContext) {
    return value.doubleValue() % multiple == 0;
  }
    <#break>
  </#if>
</#list>
}