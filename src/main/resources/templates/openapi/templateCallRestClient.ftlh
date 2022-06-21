package <#if packageApi??>${packageApi}<#elseif package??> ${package}</#if>;

import ${packageClient}.ApiRestClient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

<#assign imports=[]>
<#list pathObjects as path>
  <#list path.operationObjects as operation>
    <#list operation.requestObjects as request>
      <#list request.contentObjects as content>
        <#if content.importName?? && (!imports?seq_contains(content.importName))>
          <#if (!checkBasicTypes?seq_contains(content.importName))>
        		<#assign imports = imports+[content.importName]>
          </#if>
        </#if>
      </#list>
    </#list>
    <#list operation.responseObjects as response>
      <#list response.contentObjects as content>
        <#if content.importName?? && (!imports?seq_contains(content.importName))>
          <#if (!checkBasicTypes?seq_contains(content.importName))>
            <#assign imports = imports+[content.importName]>
          </#if>
        </#if>
      </#list>
    </#list>
  </#list>
</#list>
<#list imports as import>
import <#if packageModel??>${packageModel}<#elseif package??>${package}</#if>.${import};
</#list>

import ${packageAuth}.Authentication;
<#if authObject.securityRequirements?has_content>
  <#list authObject.securityRequirements as authSecurity>
import ${packageAuth}.${authSecurity?cap_first};
  </#list>
</#if>

import org.springframework.stereotype.Component;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;

@Component()
public class ${className?cap_first}Api {

  private ApiRestClient apiRestClient;

  private Map<String, Authentication> authenticationsApi;

  public ${className?cap_first}Api() {
    this.init();
  }

  protected void init() {
    this.authenticationsApi = new HashMap<String, Authentication>();
     <#if authObject.securityRequirements?has_content>
        <#list authObject.securityRequirements as authSecurity>
          <#if pathObjects[0].globalObject.authSchemas?has_content>
              <#list pathObjects[0].globalObject.authSchemas as authSchema>
                <#if authSecurity == authSchema.type>
    this.authenticationsApi.put("${authSchema.name}", new ${authSchema.type?cap_first}(<#if authSchema.type == "HttpBearerAuth">"${authSchema.bearerSchema}"<#elseif authSchema.type == "ApiKeyAuth">"${authSchema.apiKeyPlace}", "${authSchema.apiKeyParam}"</#if>));
                </#if>
              </#list>
          </#if>
        </#list>
      </#if>
    this.apiRestClient = new ApiRestClient(authenticationsApi);
  }

 <#list pathObjects as path>
  <#list path.operationObjects as operation>
  /**
  * ${operation.operationType} ${path.pathName}: ${operation.summary}
  <#if operation.responseObjects?has_content>
   <#if operation.parameterObjects?has_content>
  * <#list operation.parameterObjects as parameter>@param ${parameter.name} ${parameter.description} ${parameter.required?c}</#list>
   </#if>
   <#if path.parameterObjects?has_content>
  * <#list path.parameterObjects as parameter>@param ${parameter.name} ${parameter.description} ${parameter.required?c}</#list>
   </#if>
    <#if operation.requestObjects?has_content>
  *<#list operation.requestObjects as request><#list request.contentObjects as content> @param ${content.refName?uncap_first}<#if content?has_next>, </#if></#list> ${request.description! ""} <#if request.required == true>(required)</#if></#list>
   </#if>
  * @return<#list operation.responseObjects as response><#if response.responseName != "default"> ${response.description}; (status code ${response.responseName})</#if></#list>
  </#if>
  * @throws RestClientException if an error occurs while attempting to invoke the API
  */
  public <@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??>
    <#if operation.responseObjects[0].contentObjects[0].typeData?? && (operation.responseObjects[0].contentObjects[0].typeData == "array")>
    List<${operation.responseObjects[0].contentObjects[0].refName?cap_first}>
    <#elseif operation.responseObjects[0].contentObjects[0].typeData?? && (operation.responseObjects[0].contentObjects[0].typeData == "map")>
    Map<String, ${operation.responseObjects[0].contentObjects[0].refName?cap_first}>
    <#else>
    ${operation.responseObjects[0].contentObjects[0].refName?cap_first}
    </#if>
    <#else>Void</#if></@compress> ${operation.operationId}<#compress>(<#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter>${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter>${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObjects as content><#if content.typeData?? && (content.typeData == "array")>List<${content.refName?cap_first}> <#elseif content.typeData?? && (content.typeData == "map")>Map<String, ${content.refName?cap_first}> <#else>${content.refName?cap_first} </#if>${content.refName?uncap_first} <#if content?has_next>, </#if></#list></#list></#if>)</#compress> throws RestClientException {
    return ${operation.operationId}WithHttpInfo(<#compress><#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter>${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter>${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObjects as content>${content.refName?uncap_first}<#if content?has_next>, </#if></#list></#list></#if>)<#if operation.responseObjects[0].contentObjects[0]??>.getBody()</#if>;</#compress>
  }

  public <@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??>
    <#if operation.responseObjects[0].contentObjects[0].typeData?? && operation.responseObjects[0].contentObjects[0].typeData == "map">
    ResponseEntity<Map<String, ${operation.responseObjects[0].contentObjects[0].refName?cap_first}>>
    <#elseif operation.responseObjects[0].contentObjects[0].typeData?? && operation.responseObjects[0].contentObjects[0].typeData == "array">
    ResponseEntity<List<${operation.responseObjects[0].contentObjects[0].refName?cap_first}>>
    <#else>ResponseEntity<${operation.responseObjects[0].contentObjects[0].refName?cap_first}></#if>
    <#else>ResponseEntity<Void></#if></@compress> ${operation.operationId}WithHttpInfo<#compress>(<#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter>${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter>${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObjects as content><#if content.typeData?? && (content.typeData == "array")>List<${content.refName?cap_first}> <#elseif content.typeData?? && (content.typeData == "map")>Map<String, ${content.refName?cap_first}> <#else>${content.refName?cap_first} </#if> ${content.refName?uncap_first}<#if content?has_next>, </#if></#list></#list></#if>)</#compress> throws RestClientException {

    Object postBody = <#if operation.requestObjects?has_content>${operation.requestObjects[0].contentObjects[0].refName?uncap_first}<#else>null</#if>;
    <#if operation.requestObjects?has_content>
    if (${operation.requestObjects[0].contentObjects[0].refName?uncap_first} == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''${operation.requestObjects[0].contentObjects[0].refName?uncap_first}'' when calling ${operation.operationId}");
    }
    </#if>
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
       <#if parameter.in == "path">
    uriVariables.put("${parameter.name}", <#if parameter.isCollection == true> apiRestClient.collectionPathParameterToString(ApiRestClient.CollectionFormat.valueOf("csv".toUpperCase()), ${parameter.name})<#else> ${parameter.name}</#if>);
       </#if>
      </#list>
    </#if>
    <#if path.parameterObjects?has_content>
      <#list path.parameterObjects as parameter>
       <#if parameter.in == "path">
    uriVariables.put("${parameter.name}", <#if parameter.isCollection == true> apiRestClient.collectionPathParameterToString(ApiRestClient.CollectionFormat.valueOf("csv".toUpperCase()), ${parameter.name})<#else> ${parameter.name}</#if>);
       </#if>
      </#list>
    </#if>
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "query">

    queryParams.putAll(apiRestClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiRestClient.CollectionFormat.valueOf("{{{.}}}".toUpperCase(Locale.ROOT))<#else> null</#if>, "${parameter.name}", ${parameter.name}));
        </#if>
      </#list>
     </#if>
    <#if path.parameterObjects?has_content>
      <#list path.parameterObjects as parameter>
        <#if parameter.in == "query">

    queryParams.putAll(apiRestClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiRestClient.CollectionFormat.valueOf("{{{.}}}".toUpperCase(Locale.ROOT))<#else> null</#if>, "${parameter.name}", ${parameter.name}));
    </#if>
      </#list>
    </#if>
    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "header">
    if (${parameter.name} != null){
      headerParams.add("${parameter.name}", apiRestClient.parameterToString(${parameter.name}));
    }
        </#if>
      </#list>
    </#if>
    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "cookie">

    cookieParams.putAll(apiRestClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiRestClient.CollectionFormat.valueOf("{{{.}}}".toUpperCase(Locale.ROOT))<#else> null</#if>, "${parameter.name}", ${parameter.name}));

        </#if>
      </#list>
    </#if>
    <#if path.parameterObjects?has_content>
      <#list path.parameterObjects as parameter>
        <#if parameter.in == "header">
    if (${parameter.name} != null){
      headerParams.add("${parameter.name}", apiRestClient.parameterToString(${parameter.name}));
    }
        </#if>
      </#list>
    </#if>
    <#if path.parameterObjects?has_content>
      <#list path.parameterObjects as parameter>
        <#if parameter.in == "cookie">

    cookieParams.putAll(apiRestClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiRestClient.CollectionFormat.valueOf("{{{.}}}".toUpperCase(Locale.ROOT))<#else> null</#if>, "${parameter.name}", ${parameter.name}));

        </#if>
      </#list>
    </#if>
    final String[] localVarAccepts = {<#compress><#if operation.produces?has_content>
      <#list operation.produces as produce>"${produce}"<#if produce?has_next>, </#if></#list>
      </#if></#compress>};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {<#compress>
      <#if operation.consumes?has_content>
        <#list operation.consumes as consume>"${consume}"<#if consume?has_next>, </#if></#list>
      </#if></#compress>};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {<#compress>
      <#if operation.securities?has_content>
        <#list operation.securities as security>"${security}"<#if security?has_next>, </#if></#list>
      </#if></#compress>};

    ParameterizedTypeReference<@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??><#if operation.responseObjects[0].contentObjects[0].typeData?? && operation.responseObjects[0].contentObjects[0].typeData == "map">
    <Map<String, ${operation.responseObjects[0].contentObjects[0].refName?cap_first}>> localVarReturnType = new ParameterizedTypeReference<Map<String, ${operation.responseObjects[0].contentObjects[0].refName?cap_first}>>() {};
    <#elseif  operation.responseObjects[0].contentObjects[0].typeData?? && operation.responseObjects[0].contentObjects[0].typeData == "array">
    <List<${operation.responseObjects[0].contentObjects[0].refName?cap_first}>> localVarReturnType = new ParameterizedTypeReference<List<${operation.responseObjects[0].contentObjects[0].refName?cap_first}>>() {};
    <#else><${operation.responseObjects[0].contentObjects[0].refName?cap_first}> localVarReturnType = new ParameterizedTypeReference<${operation.responseObjects[0].contentObjects[0].refName?cap_first}>() {};
    </#if>
    <#else>
    <Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    </#if></@compress>

    return apiRestClient.invokeAPI("${path.globalObject.url}","${path.pathName}", HttpMethod.${operation.operationType}, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

     </#list>
    </#list>
}