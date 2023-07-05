package ${subscribePackage};

import ${method.classNamespace}.${method.className}<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>;
<#if method.keyClassNamespace?has_content>
import ${method.keyClassNamespace}.${method.keyClassName};
</#if>
import ${subscribePackage}.MessageWrapper;

public interface I${method.operationId?cap_first} {

  void ${method.operationId?uncap_first}(final MessageWrapper<${method.className}<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>, ${method.keyClassName}> value);
}