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
public class MaxValidator implements ConstraintValidator<Max, BigDecimal> {

    private BigDecimal maximum;
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
public class MaxValidator implements ConstraintValidator<Max, Integer> {

    private int maximum;
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
public class MaxValidator implements ConstraintValidator<Max, Float> {

    private float maximum;
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
public class MaxValidator implements ConstraintValidator<Max, Double> {

    private double maximum;
    </#if>
</#list>
    private boolean exclusive;

    @Override
    public void initialize(Max constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
<#list schema.fieldObjectList as field>
    <#if field.dataType.baseType?has_content && (field.dataType.baseType == "BigDecimal" || field.dataType.baseType == "bigDecimal")>
        this.maximum = new BigDecimal(constraintAnnotation.maximum());
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
        this.maximum = Integer.parseInt(constraintAnnotation.maximum());
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
        this.maximum = Float.parseFloat(constraintAnnotation.maximum());
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
        this.maximum = Double.parseDouble(constraintAnnotation.maximum());
    </#if>
</#list>
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
<#list schema.fieldObjectList as field>
    <#if field.dataType.baseType?has_content && (field.dataType.baseType == "BigDecimal" || field.dataType.baseType == "bigDecimal")>
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value < this.maximum || (!exclusive && value == this.maximum));
    }
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Integer" || field.dataType.baseType == "integer")>
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.intValue() < this.maximum || (!exclusive && value.intValue() == this.maximum));
    }
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Float" || field.dataType.baseType == "float")>
    public boolean isValid(Float value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.floatValue() < this.maximum || (!exclusive && value.floatValue() == this.maximum));
    }
        <#break>
    <#elseif field.dataType.baseType?has_content && (field.dataType.baseType == "Double" || field.dataType.baseType == "double")>
    public boolean isValid(Double value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.doubleValue() < this.maximum || (!exclusive && value.doubleValue() == this.maximum));
    }
    </#if>
</#list>
}