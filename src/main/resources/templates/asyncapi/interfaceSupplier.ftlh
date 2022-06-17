package ${supplierPackage};

import ${method.classNamespace}<#if supplierEntitiesPostfix?has_content>${supplierEntitiesPostfix}</#if>;

public interface I${method.operationId?cap_first} {

    ${method.className}<#if supplierEntitiesPostfix?has_content>${supplierEntitiesPostfix}</#if> ${method.operationId?uncap_first}();
}