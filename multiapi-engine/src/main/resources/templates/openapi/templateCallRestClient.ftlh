<#ftl output_format="plainText">
package <#if packageApi??>${packageApi}<#elseif package??> ${package}</#if>;

<#assign imports=[]>
<#assign model_imports=[]>
<#list pathObjects as path>
  <#list path.operationObjects as operation>
    <#list operation.requestObjects as request>
      <#list request.contentObjects as content>
        <#if content.importName?? && (!imports?seq_contains(content.importName)) && (!model_imports?seq_contains(content.importName))>
          <#if (!checkBasicTypes?seq_contains(content.importName))>
            <#if (!content.importName?contains("java."))>
              <#assign model_imports = model_imports+[content.importName]>
            <#else>
              <#assign imports = imports+[content.importName]>
            </#if>
          </#if>
        </#if>
      </#list>
    </#list>
    <#list operation.responseObjects as response>
      <#list response.contentObjects as content>
        <#if content.importName?? && (!imports?seq_contains(content.importName)) && (!model_imports?seq_contains(content.importName))>
          <#if (!checkBasicTypes?seq_contains(content.importName))>
            <#if (!content.importName?contains("java."))>
              <#assign model_imports = model_imports+[content.importName]>
            <#else>
              <#assign imports = imports+[content.importName]>
            </#if>
          </#if>
        </#if>
      </#list>
    </#list>
    <#list operation.parameterObjects as parameter>
      <#if parameter.importName?? && (!imports?seq_contains(parameter.importName)) && (!model_imports?seq_contains(parameter.importName))>
        <#if (!checkBasicTypes?seq_contains(parameter.importName))>
          <#if (!parameter.importName?contains("java."))>
            <#assign model_imports = model_imports+[parameter.importName]>
          <#else>
            <#assign imports = imports+[parameter.importName]>
          </#if>
        </#if>
      </#if>
    </#list>
  </#list>
</#list>
<#list imports as import>
import ${import};
</#list>
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import ${packageClient}.ApiRestClient;

<#list model_imports as import>
import <#if packageModel??>${packageModel}<#elseif package??>${package}</#if>.${import};
</#list>

import ${packageAuth}.Authentication;
<#if authObject.securityRequirements?has_content>
  <#list authObject.securityRequirements as authSecurity>
import ${packageAuth}.${authSecurity?cap_first};
  </#list>
</#if>

<#if clientComponent>
import org.springframework.stereotype.Component;
</#if>
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

<#if clientComponent>
@Component()
</#if>
public class ${className?cap_first}Api {

  private ApiRestClient apiRestClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "${pathObjects[0].globalObject.url!""}";

  /**
   * Builds its own ApiRestClient, sending requests to the contract's first server, and is the constructor Spring uses when the class is a component.
   */
  public ${className?cap_first}Api() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured RestTemplate, to the contract's first server.
   */
  public ${className?cap_first}Api(final ApiRestClient apiRestClient) {
    this.apiRestClient = Objects.requireNonNull(apiRestClient, "apiRestClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * root URI (RestTemplateBuilder.rootUri(...)).
   */
  public ${className?cap_first}Api(final ApiRestClient apiRestClient, final String basePath) {
    this(apiRestClient);
    this.basePath = basePath;
  }

  public String getBasePath() {
    return basePath;
  }

  public void setBasePath(final String basePath) {
    this.basePath = basePath;
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
   * ${operation.operationType} ${path.pathName}<#if operation.summary?has_content>: ${operation.summary}</#if>
  <#if operation.responseObjects?has_content>
   <#if operation.parameterObjects?has_content>
<#list operation.parameterObjects as parameter>
   * @param ${parameter.variableName} ${parameter.description!" "} <#if parameter.required == true>(required)</#if>
</#list>
   </#if>
   <#if path.parameterObjects?has_content>
  <#list path.parameterObjects as parameter>
    * @param ${parameter.variableName} ${parameter.description!" "} <#if parameter.required == true>(required)</#if>
  </#list>
   </#if>
    <#if operation.requestObjects?has_content>
  <#list operation.requestObjects as request><#list request.contentObjects as content>
   * @param ${content.dataType?api.getVariableNameString()}<#if content?has_next>, </#if></#list> ${request.description! ""} <#if request.required == true>(required)</#if>
  </#list>
   </#if>
   * @return<#list operation.responseObjects as response><#if response.responseName != "default"> ${response.description!" "}; (status code ${response.responseName})</#if></#list>
  </#if>
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public <@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??>
    ${operation.responseObjects[0].contentObjects[0].dataType}
    <#else>void</#if></@compress> ${operation.operationId}<#compress>(<#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter>${parameter.dataType} ${parameter.variableName}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter>${parameter.dataType} ${parameter.variableName}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObjects as content>${content.dataType} ${content.dataType?api.getVariableNameString()} <#if content?has_next>, </#if></#list></#list></#if>)</#compress> throws RestClientException {
    <#if operation.responseObjects[0].contentObjects[0]??>return </#if>${operation.operationId}WithHttpInfo(<#compress><#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter>${parameter.variableName}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter>${parameter.variableName}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObjects as content>${content.dataType?api.getVariableNameString()}<#if content?has_next>, </#if></#list></#list></#if>)<#if operation.responseObjects[0].contentObjects[0]??>.getBody()</#if>;</#compress>
  }

  public <@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??>
    ResponseEntity<${operation.responseObjects[0].contentObjects[0].dataType}>
    <#else>ResponseEntity<Void></#if></@compress> ${operation.operationId}WithHttpInfo<#compress>(<#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter>${parameter.dataType} ${parameter.variableName}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter>${parameter.dataType} ${parameter.variableName}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if operation.requestObjects?has_content><#list operation.requestObjects as request><#list request.contentObjects as content>${content.dataType} ${content.dataType?api.getVariableNameString()}<#if content?has_next>, </#if></#list></#list></#if>)</#compress> throws RestClientException {

    Object postBody = <#if operation.requestObjects?has_content>${operation.requestObjects[0].contentObjects[0].dataType?api.getVariableNameString()}<#else>null</#if>;
    <#if operation.requestObjects?has_content>
    if (${operation.requestObjects[0].contentObjects[0].dataType?api.getVariableNameString()} == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''${operation.requestObjects[0].contentObjects[0].dataType?api.getVariableNameString()}'' when calling ${operation.operationId}");
    }
    </#if>
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
       <#if parameter.in == "path">
    uriVariables.put("${parameter.name}", <#if parameter.isCollection == true> apiRestClient.collectionPathParameterToString(ApiRestClient.CollectionFormat.valueOf("csv".toUpperCase()), ${parameter.variableName})<#else> ${parameter.variableName}</#if>);
       </#if>
      </#list>
    </#if>
    <#if path.parameterObjects?has_content>
      <#list path.parameterObjects as parameter>
       <#if parameter.in == "path">
    uriVariables.put("${parameter.name}", <#if parameter.isCollection == true> apiRestClient.collectionPathParameterToString(ApiRestClient.CollectionFormat.valueOf("csv".toUpperCase()), ${parameter.variableName})<#else> ${parameter.variableName}</#if>);
       </#if>
      </#list>
    </#if>
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    <#if operation.requestObjects?has_content>
        <#if operation.requestObjects[0].contentObjects[0].schemaObject?has_content>
             <#list operation.requestObjects[0].contentObjects[0].schemaObject.fieldObjectList as field>
    formParams.put("${field.baseName}", List.of(${operation.requestObjects[0].contentObjects[0].dataType?api.getVariableNameString()}.get${field.fieldName?cap_first}()));
             </#list>
         </#if>
    </#if>

    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "query">
    <#if parameter.dataType.baseType == "object" && parameter.dataType.innerType??>queryParams.putAll(apiRestClient.objectToQueryParams("${parameter.effectiveStyle}", ${parameter.effectiveExplode?c}, "${parameter.name}", ${parameter.variableName}));<#else>queryParams.putAll(apiRestClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiRestClient.CollectionFormat.${parameter.collectionFormat}<#else> null</#if>, "${parameter.name}", ${parameter.variableName}));</#if>
        </#if>
      </#list>
     </#if>
    <#if path.parameterObjects?has_content>
      <#list path.parameterObjects as parameter>
        <#if parameter.in == "query">

    <#if parameter.dataType.baseType == "object" && parameter.dataType.innerType??>queryParams.putAll(apiRestClient.objectToQueryParams("${parameter.effectiveStyle}", ${parameter.effectiveExplode?c}, "${parameter.name}", ${parameter.variableName}));<#else>queryParams.putAll(apiRestClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiRestClient.CollectionFormat.${parameter.collectionFormat}<#else> null</#if>, "${parameter.name}", ${parameter.variableName}));</#if>
    </#if>
      </#list>
    </#if>
    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "header">
    if (${parameter.variableName} != null) {
      headerParams.add("${parameter.name}", apiRestClient.parameterToString(${parameter.variableName}));
    }
        </#if>
      </#list>
    </#if>
    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "cookie">

    cookieParams.putAll(apiRestClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiRestClient.CollectionFormat.${parameter.collectionFormat}<#else> null</#if>, "${parameter.name}", ${parameter.variableName}));

        </#if>
      </#list>
    </#if>
    <#if path.parameterObjects?has_content>
      <#list path.parameterObjects as parameter>
        <#if parameter.in == "header">
    if (${parameter.variableName} != null) {
      headerParams.add("${parameter.name}", apiRestClient.parameterToString(${parameter.variableName}));
    }
        </#if>
      </#list>
    </#if>
    <#if path.parameterObjects?has_content>
      <#list path.parameterObjects as parameter>
        <#if parameter.in == "cookie">

    cookieParams.putAll(apiRestClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiRestClient.CollectionFormat.${parameter.collectionFormat}<#else> null</#if>, "${parameter.name}", ${parameter.variableName}));

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

    ParameterizedTypeReference<@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??>
    <${operation.responseObjects[0].contentObjects[0].dataType}> localVarReturnType = new ParameterizedTypeReference<${operation.responseObjects[0].contentObjects[0].dataType}>() {};
    <#else>
    <Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    </#if></@compress>

    return apiRestClient.invokeAPI(basePath,"${path.pathName}", HttpMethod.${operation.operationType}, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

     </#list>
    </#list>
}