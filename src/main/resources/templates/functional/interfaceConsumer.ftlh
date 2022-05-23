package ${subscribePackage};

import ${method.classNamespace}<#if subscribeEntitiesPostfix?has_content>${subscribeEntitiesPostfix}</#if>;

public interface I${method.operationId?cap_first} {

    void ${method.operationId?uncap_first}(${method.className}<#if subscribeEntitiesPostfix?has_content>${subscribeEntitiesPostfix}</#if> value);
}