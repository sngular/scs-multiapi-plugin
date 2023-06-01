package ${subscribePackage};

import ${method.classNamespace}<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>;
import ${subscribePackage}.MessageWrapper<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>;

public interface I${method.operationId?cap_first} {

  void ${method.operationId?uncap_first}(final MessageWrapper<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if> value);
}