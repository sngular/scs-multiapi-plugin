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
public class MaxValidator implements ConstraintValidator<Max, BigDecimal> {

    private BigDecimal maximum;
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
public class MaxValidator implements ConstraintValidator<Max, Integer> {

    private int maximum;
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
public class MaxValidator implements ConstraintValidator<Max, Float> {

    private float maximum;
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
    public class MaxValidator implements ConstraintValidator<Max, Double> {

    private double maximum;
    </#if>
</#list>
    private boolean exclusive;

    @Override
    public void initialize(Max constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigDecimal")>
        this.maximum = new BigDecimal(constraintAnnotation.maximum());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
        this.maximum = Integer.parseInt(constraintAnnotation.maximum());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
        this.maximum = Float.parseFloat(constraintAnnotation.maximum());
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
        this.maximum = Double.parseDouble(constraintAnnotation.maximum());
    </#if>
</#list>
        this.exclusive = constraintAnnotation.exclusive();
    }

    @Override
<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigDecimal")>
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value < this.maximum || (!exclusive && value == this.maximum));
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.intValue() < this.maximum || (!exclusive && value.intValue() == this.maximum));
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
    public boolean isValid(Float value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.floatValue() < this.maximum || (!exclusive && value.floatValue() == this.maximum));
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
    public boolean isValid(Double value, ConstraintValidatorContext context) {
        return Objects.isNull(value) || (value.doubleValue() < this.maximum || (!exclusive && value.doubleValue() == this.maximum));
    }
    </#if>
</#list>
}