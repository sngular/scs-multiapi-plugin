package ${supplierPackage};

import ${method.classNamespace}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>;
import ${supplierPackage}.MessageWrapper<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>;

public interface I${method.operationId?cap_first} {

  MessageWrapper<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if> ${method.operationId?uncap_first}(String key);
}