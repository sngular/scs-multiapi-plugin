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
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigdecimal")>
public class MinValidator implements ConstraintValidator<Min, BigDecimal> {

    private BigDecimal minimum;
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
public class MinValidator implements ConstraintValidator<Min, Integer> {

    private int minimum;
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
public class MinValidator implements ConstraintValidator<Min, Float> {

    private float minimum;
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
public class MinValidator implements ConstraintValidator<Min, Double> {

    private double minimum;
    </#if>
</#list>
    private boolean exclusive;

    @Override
    public void initialize(Min constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
<#list schema.fieldObjectList as field>
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigdecimal")>
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
    <#if field.dataTypeSimple?has_content && (field.dataTypeSimple == "BigDecimal" || field.dataTypeSimple == "bigdecimal")>
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        if (Objects.isNull(value)) {
            return true;
        }
        if (exclusive) {
            if (value <= this.minimum) {
                return false;
            } else {
                return true;
            }
        }
        if (value < this.minimum) {
            return false;
        } else {
            return true;
        }
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Integer" || field.dataTypeSimple == "integer")>
    public boolean isValid(Integer value, ConstraintValidatorContext context) {
        if (Objects.isNull(value)) {
            return true;
        }
        if (exclusive) {
            if (value.intValue() <= this.minimum) {
                return false;
            } else {
                return true;
            }
        }
        if (value.intValue() < this.minimum) {
            return false;
        } else {
            return true;
        }
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Float" || field.dataTypeSimple == "float")>
    public boolean isValid(Float value, ConstraintValidatorContext context) {
        if (Objects.isNull(value)) {
            return true;
        }
        if (exclusive) {
            if (value.floatValue() <= this.minimum) {
                return false;
            } else {
                return true;
            }
        }
        if (value.floatValue() < this.minimum) {
            return false;
        } else {
            return true;
        }
    }
        <#break>
    <#elseif field.dataTypeSimple?has_content && (field.dataTypeSimple == "Double" || field.dataTypeSimple == "double")>
    public boolean isValid(Double value, ConstraintValidatorContext context) {
        if (Objects.isNull(value)) {
            return true;
        }
        if (exclusive) {
            if (value.doubleValue() <= this.minimum) {
                return false;
            } else {
                return true;
            }
        }
        if (value.doubleValue() < this.minimum) {
            return false;
        } else {
            return true;
        }
    }
    </#if>
</#list>
}