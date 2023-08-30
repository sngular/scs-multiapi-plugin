package ${packageModel}.${schema.parentPackage}.customvalidator;

<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && field.dataTypeSimple == "BigDecimal">
        import java.math.BigDecimal;
    </#if>
</#list>
import java.util.Objects;

import ${javaEEPackage}.validation.ConstraintValidator;
import ${javaEEPackage}.validation.ConstraintValidatorContext;

<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigDecimal")>
public class MinValidator implements ConstraintValidator<Min, BigDecimal> {

    private BigDecimal minimum;
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
public class MinValidator implements ConstraintValidator<Min, Integer> {

    private int minimum;
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
public class MinValidator implements ConstraintValidator<Min, Float> {

    private float minimum;
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
public class MinValidator implements ConstraintValidator<Min, Double> {

    private double minimum;
        <#break>
    </#if>
</#list>
    private boolean exclusive;

    @Override
    public void initialize(Min constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigDecimal")>
        this.minimum = new BigDecimal(constraintAnnotation.minimum());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
        this.minimum = Integer.parseInt(constraintAnnotation.minimum());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
        this.minimum = Float.parseFloat(constraintAnnotation.minimum());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
        this.minimum = Double.parseDouble(constraintAnnotation.minimum());
    </#if>
</#list>
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigDecimal")>
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value > this.minimum || (!exclusive && value == this.minimum));
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.intValue() > this.minimum || (!exclusive && value.intValue() == this.minimum));
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
    public boolean isValid(Float value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.floatValue() > this.minimum || (!exclusive && value.floatValue() == this.minimum));
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
    public boolean isValid(Double value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.doubleValue() > this.minimum || (!exclusive && value.doubleValue() == this.minimum));
    }
    </#if>
</#list>
}