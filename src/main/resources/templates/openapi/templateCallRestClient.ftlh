package <#if packageApi??>${packageApi}<#elseif package??> ${package}</#if>;

import ${packageClient}.ApiRestClient;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

<#assign imports=[]>
<#list pathObject as path>
  <#list path.operationObject as operation>
    <#list operation.requestObjects as request>
      <#list request.contentObject as content>
        <#if (!imports?seq_contains(content.refNameObject.refName))>
          <#if content.refNameObject.checkImport>
        		<#assign imports = imports+[content.refName]>
          </#if>
        </#if>
      </#list>
    </#list>
    <#list operation.responseObjects as response>
      <#list response.contentObject as content>
        <#if (!imports?seq_contains(content.refNameObject.refName))>
          <#if content.refNameObject.checkImport>
            <#assign imports = imports+[content.refNameObject.refName]>
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

    private Map< String, Authentication> authenticationsApi;

    public ${className?cap_first}Api() {
       this.init();
    }

    protected void init() {
      this.authenticationsApi = new HashMap< String, Authentication>();
     <#if authObject.securityRequirements?has_content>
        <#list authObject.securityRequirements as authSecurity>
          <#if pathObject[0].globalObjects.authSchemas?has_content>
              <#list pathObject[0].globalObjects.authSchemas as authSchema>
                <#if authSecurity == authSchema.type>
      this.authenticationsApi.put("${authSchema.name}", new ${authSchema.type?cap_first}(<#if authSchema.type == "HttpBearerAuth">"${authSchema.bearerSchema}"<#elseif authSchema.type == "ApiKeyAuth">"${authSchema.apiKeyPlace}", "${authSchema.apiKeyParam}"</#if>));
                </#if>
              </#list>
          </#if>
        </#list>
      </#if>
      this.apiRestClient = new ApiRestClient(authenticationsApi);
    }


    <#list pathObject as path>
     <#list path.operationObject as operation>

     /**
     * ${operation.operationType} ${path.pathName} : ${operation.summary}
     <#if operation.responseObjects?has_content>
      <#if operation.parameterObjects?has_content>
     * @param <#list operation.parameterObjects as parameter> ${parameter.name} ${parameter.description} ${parameter.required?c}</#list>
      </#if>
       <#if operation.requestObjects?has_content>
     *<#list operation.requestObjects as request><#list request.contentObject as content> @param ${content.refNameObject.refName?uncap_first} <#if content?has_next>, </#if></#list> ${request.description! ""} <#if request.required == true> (required)</#if></#list>
      </#if>
     * @return<#list operation.responseObjects as response><#if response.responseName != "default">  ${response.description}; (status code ${response.responseName})</#if></#list>
     </#if>
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
      public <#compress><#if operation.responseObjects[0].contentObject[0]??>
          <#if operation.responseObjects[0].contentObject[0].typeData?? && (operation.responseObjects[0].contentObject[0].typeData == "array")>
          ArrayList<${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>
          <#elseif operation.responseObjects[0].contentObject[0].typeData?? && (operation.responseObjects[0].contentObject[0].typeData == "map")>
          HashMap< String,${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>
          <#else>
          ${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}
          </#if>
          <#else>Void</#if></#compress> ${operation.operationId} <#compress>(<#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> ${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObject as content><#if content.typeData?? && (content.typeData == "array")> ArrayList<${content.refNameObject.refName?cap_first}> <#elseif content.typeData?? && (content.typeData == "map")> HashMap< String,${content.refNameObject.refName?cap_first}> <#else> ${content.refNameObject.refName?cap_first} </#if> ${content.refNameObject.refName?uncap_first} <#if content?has_next>, </#if></#list></#list></#if>)</#compress> throws RestClientException {
        return ${operation.operationId}WithHttpInfo(<#compress><#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>,</#if></#list> </#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObject as content> ${content.refName?uncap_first}<#if content?has_next>, </#if></#list></#list></#if>)<#if operation.responseObjects[0].contentObject[0]??>.getBody()</#if>;</#compress>
      }


     public <#compress><#if operation.responseObjects[0].contentObject[0]??>
        <#if operation.responseObjects[0].contentObject[0].typeData?? && operation.responseObjects[0].contentObject[0].typeData == "map">
                 ResponseEntity< HashMap< String,${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>>
        <#elseif  operation.responseObjects[0].contentObject[0].typeData?? && operation.responseObjects[0].contentObject[0].typeData == "array">
                 ResponseEntity< ArrayList< ${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>>
        <#else>ResponseEntity<${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}></#if>
        <#else> ResponseEntity< Void></#if></#compress> ${operation.operationId}WithHttpInfo <#compress>(<#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> ${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObject as content><#if content.typeData?? && (content.typeData == "array")> ArrayList<${content.refNameObject.refName?cap_first}> <#elseif content.typeData?? && (content.typeData == "map")> HashMap< String,${content.refNameObject.refName?cap_first}> <#else> ${content.refNameObject.refName?cap_first} </#if> ${content.refNameObject.refName?uncap_first} <#if content?has_next>, </#if></#list></#list></#if>)</#compress> throws RestClientException {
         Object postBody = <#if operation.requestObjects?has_content>${operation.requestObjects[0].contentObject[0].refNameObject.refName?uncap_first}<#else>null</#if>;
          <#if operation.requestObjects?has_content>
         if (${operation.requestObjects[0].contentObject[0].refNameObject.refName?uncap_first} == null) {
            throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''${operation.requestObjects[0].contentObject[0].refNameObject.refName?uncap_first}'' when calling ${operation.operationId}");
         }
          </#if>
          // create path and map variables
         final Map< String, Object> uriVariables = new HashMap< String, Object>();

          <#if operation.parameterObjects?has_content>
            <#list operation.parameterObjects as parameter>
             <#if parameter.in == "path">
         uriVariables.put("${parameter.name}", <#if parameter.isCollection == true> apiRestClient.collectionPathParameterToString(ApiRestClient.CollectionFormat.valueOf("csv".toUpperCase()), ${parameter.name})<#else> ${parameter.name}</#if>);
             </#if>
            </#list>
          </#if>
         final MultiValueMap< String, String> queryParams = new LinkedMultiValueMap< String, String>();
         final HttpHeaders headerParams = new HttpHeaders();
         final MultiValueMap< String, String> cookieParams = new LinkedMultiValueMap< String, String>();
         final MultiValueMap< String, Object> formParams = new LinkedMultiValueMap< String, Object>();

          <#if operation.parameterObjects?has_content>
            <#list operation.parameterObjects as parameter>
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

         final String[] localVarAccepts = {<#compress><#if operation.produces?has_content>
             <#list operation.produces as produce>"${produce}"<#if produce?has_next>, </#if></#list>
          </#if></#compress>};
         final List< MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
         final String[] localVarContentTypes = {<#compress>
          <#if operation.consumes?has_content>
            <#list operation.consumes as consume>"${consume}"<#if consume?has_next>, </#if></#list>
          </#if></#compress>};
         final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

         String[] localVarAuthNames = new String[] {<#compress>
          <#if operation.security?has_content>
            <#list operation.security as security>"${security}"<#if security?has_next>, </#if></#list>
          </#if></#compress>};

         ParameterizedTypeReference<#compress><#if operation.responseObjects[0].contentObject[0]??><#if operation.responseObjects[0].contentObject[0].typeData?? && operation.responseObjects[0].contentObject[0].typeData == "map">
         < HashMap< String,${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>> localVarReturnType = new ParameterizedTypeReference< HashMap< String,${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>>() {};
         <#elseif  operation.responseObjects[0].contentObject[0].typeData?? && operation.responseObjects[0].contentObject[0].typeData == "array">
         < ArrayList< ${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>> localVarReturnType = new ParameterizedTypeReference< ArrayList< ${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>>() {};
         <#else><${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}> localVarReturnType = new ParameterizedTypeReference<${operation.responseObjects[0].contentObject[0].refNameObject.refName?cap_first}>() {};
         </#if>
         <#else>
         < Void> localVarReturnType = new ParameterizedTypeReference< Void>() {};
         </#if></#compress>

         return apiRestClient.invokeAPI("${path.globalObjects.url}","${path.pathName}", HttpMethod.${operation.operationType}, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

     }


     </#list>
    </#list>

}
