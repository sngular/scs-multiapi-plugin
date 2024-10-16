package ${supplierPackage};

import ${method.classNamespace}.${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>;
<#if method.keyClassNamespace?has_content>
import ${method.keyClassNamespace}.${method.keyClassName};
</#if>
import ${supplierPackage}.MessageWrapper;

public interface I${method.operationId?cap_first} {

  MessageWrapper<${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>, ${method.keyClassName?cap_first}> ${method.operationId?uncap_first}();
}