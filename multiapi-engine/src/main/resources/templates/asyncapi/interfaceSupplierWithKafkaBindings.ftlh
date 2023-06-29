package ${supplierPackage};

import ${method.classNamespace}.${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>;
import ${method.keyClassNamespace}.${method.keyClassName};
import ${supplierPackage}.MessageWrapper;

public interface I${method.operationId?cap_first} {

  MessageWrapper<${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>, ${method.keyClassName}> ${method.operationId?uncap_first}();
}