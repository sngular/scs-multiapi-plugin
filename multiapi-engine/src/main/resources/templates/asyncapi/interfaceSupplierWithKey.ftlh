package ${supplierPackage};

import ${method.classNamespace}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>;

public interface I${method.operationId?cap_first} {

  ${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if> ${method.operationId?uncap_first}(final String key);
}