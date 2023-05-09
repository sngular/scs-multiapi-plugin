package ${packageModel}.customvalidator;

<#list schema.fieldObjectList as field>
    <#if field.dataType.baseType?has_content && field.dataType.baseType == "BigDecimal">
        import java.math.BigDecimal;
    </#if>
</#list>
import java.util.Objects;

import ${javaEEPackage}.validation.ConstraintValidator;
import ${javaEEPackage}.validation.ConstraintValidatorContext;

<#list schema.fieldObjectList as field>
    <#if field.dataType.baseType?has_content && (field.dataType.baseType == "BigDecimal" || field.dataType.baseType == "bigDecimal")>
public class MinValidator implements ConstraintValidator<Min, BigDecimal> {

    private BigDecimal minimum;
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
public class MinValidator implements ConstraintValidator<Min, Integer> {

    private int minimum;
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
public class MinValidator implements ConstraintValidator<Min, Float> {

    private float minimum;
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
public class MinValidator implements ConstraintValidator<Min, Double> {

    private double minimum;
    </#if>
</#list>
    private boolean exclusive;

    @Override
    public void initialize(Min constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
<#list schema.fieldObjectList as field>
    <#if field.dataType.baseType?has_content && (field.dataType.baseType == "BigDecimal" || field.dataType.baseType == "bigDecimal")>
        this.minimum = new BigDecimal(constraintAnnotation.minimum());
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
        this.minimum = Integer.parseInt(constraintAnnotation.minimum());
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
        this.minimum = Float.parseFloat(constraintAnnotation.minimum());
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
        this.minimum = Double.parseDouble(constraintAnnotation.minimum());
    </#if>
</#list>
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
<#list schema.fieldObjectList as field>
    <#if field.dataType.baseType?has_content && (field.dataType.baseType == "BigDecimal" || field.dataType.baseType == "bigDecimal")>
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value > this.minimum || (!exclusive && value == this.minimum));
    }
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.intValue() > this.minimum || (!exclusive && value.intValue() == this.minimum));
    }
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
    public boolean isValid(Float value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.floatValue() > this.minimum || (!exclusive && value.floatValue() == this.minimum));
    }
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
    public boolean isValid(Double value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.doubleValue() > this.minimum || (!exclusive && value.doubleValue() == this.minimum));
    }
    </#if>
</#list>
}