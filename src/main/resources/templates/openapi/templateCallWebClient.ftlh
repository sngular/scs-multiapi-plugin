package <#if packageApi??>${packageApi}<#elseif package??> ${package}</#if>;

import ${packageClient}.ApiWebClient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

<#assign imports=[]>
<#list pathObjects as path>
  <#list path.operationObjects as operation>
    <#list operation.requestObjects as request>
      <#list request.contentObjects as content>
        <#if (!imports?seq_contains(content.refNameObject.refName))>
          <#if content.refNameObject.checkImport>
            <#assign imports = imports+[content.refNameObject.refName]>
          </#if>
        </#if>
      </#list>
    </#list>
    <#list operation.responseObjects as response>
      <#list response.contentObjects as content>
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

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public class ${className?cap_first}Api {

  private ApiWebClient apiWebClient;

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
    this.apiWebClient = new ApiWebClient(authenticationsApi);
  }

  <#list pathObjects as path>
    <#list path.operationObjects as operation>
  /**
  * ${operation.operationType} ${path.pathName} : ${operation.summary}
  <#if operation.responseObjects?has_content>
  <#if operation.parameterObjects?has_content>
  * @param<#list operation.parameterObjects as parameter> ${parameter.name} ${parameter.description} ${parameter.required?c}</#list>
  </#if>
  <#if operation.requestObjects?has_content>
  * <#list operation.requestObjects as request><#list request.contentObject as content>@param ${content.refNameObject.refName?uncap_first}<#if content?has_next>, </#if></#list>${request.description! ""}<#if request.required == true> (required)</#if></#list>
  </#if>
  * @return<#list operation.responseObjects as response><#if response.responseName != "default"> ${response.description}; (status code ${response.responseName})</#if></#list>
  </#if>
  * @throws WebClientResponseException if an error occurs while attempting to invoke the API
  */
  private ResponseSpec ${operation.operationId}RequestCreation(<@compress single_line=true>
  <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> ${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>,</#if></#list>
  </#if><#if operation.requestObjects?has_content><#if operation.parameterObjects?has_content>, </#if><#list operation.requestObjects as request><#list request.contentObject as content>${content.refNameObject.refName?cap_first} ${content.refNameObject.refName?uncap_first}</#list></#list></#if></@compress>) throws WebClientResponseException {
    Object postBody = <#if operation.requestObjects?has_content>${operation.requestObjects[0].contentObjects[0].refNameObject.refName?uncap_first}<#else>null</#if>;
    <#if operation.requestObjects?has_content>
    if (${operation.requestObjects[0].contentObjects[0].refNameObject.refName?uncap_first} == null) {
    throw new WebClientResponseException("Missing the required parameter ''${operation.requestObjects[0].contentObjects[0].refNameObject.refName?uncap_first}'' when calling ${operation.operationId}", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
  }
    </#if>
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "path">
    pathParams.put("${parameter.name}", <#if parameter.isCollection == true> apiWebClient.collectionPathParameterToString(ApiWebClient.CollectionFormat.valueOf("csv".toUpperCase()), ${parameter.name})<#else> ${parameter.name}</#if>);
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

    queryParams.putAll(apiWebClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiWebClient.CollectionFormat.valueOf("{{{.}}}".toUpperCase(Locale.ROOT))<#else> null</#if>, "${parameter.name}", ${parameter.name}));

        </#if>
      </#list>
    </#if>
    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "header">
    if (${parameter.name} != null){
      headerParams.add("${parameter.name}", apiWebClient.parameterToString(${parameter.name}));
    }
        </#if>
      </#list>
    </#if>
    <#if operation.parameterObjects?has_content>
      <#list operation.parameterObjects as parameter>
        <#if parameter.in == "cookie">

    cookieParams.putAll(apiWebClient.parameterToMultiValueMap(<#if parameter.isCollection == true> ApiWebClient.CollectionFormat.valueOf("{{{.}}}".toUpperCase(Locale.ROOT))<#else> null</#if>, "${parameter.name}", ${parameter.name}));
        </#if>
      </#list>
    </#if>

    final String[] localVarAccepts = {<#compress><#if operation.produces?has_content>
      <#list operation.produces as produce>"${produce}"<#if produce?has_next>, </#if></#list>
      </#if></#compress>};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {<#compress>
      <#if operation.consumes?has_content>
      <#list operation.consumes as consume>"${consume}"<#if consume?has_next>, </#if></#list>
      </#if></#compress>};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {<#compress>
    <#if operation.securities?has_content>
    <#list operation.securities as security>"${security}"<#if security?has_next>, </#if></#list>
    </#if></#compress>};

    <#if operation.responseObjects[0].contentObjects[0]??>
    ParameterizedTypeReference<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}> localVarReturnType = new ParameterizedTypeReference<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}>() {};
    <#else>
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    </#if>
    return apiWebClient.invokeAPI("${path.globalObject.url}","${path.pathName}", HttpMethod.${operation.operationType}, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
  * ${operation.operationType} ${path.pathName} : ${operation.summary}
  <#if operation.responseObjects?has_content>
  <#if operation.parameterObjects?has_content>
  * @param<#list operation.parameterObjects as parameter> ${parameter.name} ${parameter.description} ${parameter.required?c}</#list>
  </#if>
  <#if operation.requestObjects?has_content>
  * <#list operation.requestObjects as request>@param <#list request.contentObject as content>${content.refNameObject.refName?uncap_first} <#if content?has_next>, </#if></#list> ${request.description! ""}<#if request.required == true> (required)</#if></#list>
  </#if>
  * @return<#list operation.responseObjects as response><#if response.responseName != "default"> ${response.description}; (status code ${response.responseName})</#if></#list>
  </#if>
  * @throws WebClientResponseException if an error occurs while attempting to invoke the API
  */
  public <@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??>
  <#if operation.responseObjects[0].contentObjects[0].typeData?? && (operation.responseObjects[0].contentObjects[0].typeData == "array" || operation.responseObjects[0].contentObjects[0].typeData == "map")>
  Flux<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}><#else>Mono<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}></#if>
  <#else>Mono<Void></#if></@compress> ${operation.operationId}(<@compress single_line=true>
  <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter>${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if>
  <#if operation.requestObjects?has_content>
  <#list operation.requestObjects as request>
  <#list request.contentObjects as content>
  <#if content.typeData?? && (content.typeData == "array")>List<${content.refNameObject.refName?cap_first}>
  <#elseif content.typeData?? && (content.typeData == "map")>Map<String,${content.refNameObject.refName?cap_first}>
  <#else>${content.refNameObject.refName?cap_first}
  </#if>${content.refNameObject.refName?uncap_first} <#if content?has_next>, </#if></#list>
  </#list></#if></@compress>) throws WebClientResponseException {
    <#if operation.responseObjects[0].contentObjects[0]??>
    ParameterizedTypeReference<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}> localVarReturnType = new ParameterizedTypeReference<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}>() {};
    <#else>
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    </#if>
    return ${operation.operationId}RequestCreation(<@compress single_line=true>
      <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>,</#if></#list> </#if>
      <#if operation.requestObjects?has_content>
      <#list operation.requestObjects as request><#list request.contentObject as content> ${content.refNameObject.refName?uncap_first} <#if content?has_next>, </#if></#list></#list></#if>
      </@compress>).<#if operation.responseObjects[0].contentObjects[0].typeData?? && (operation.responseObjects[0].contentObjects[0].typeData == "array" || operation.responseObjects[0].contentObjects[0].typeData == "map")>bodyToFlux<#else>bodyToMono</#if>(localVarReturnType);
  }

  public <@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??> Mono<<#if operation.responseObjects[0].contentObjects[0].typeData?? && operation.responseObjects[0].contentObjects[0].typeData == "map">
  ResponseEntity<Map<String,${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}>>
  <#elseif operation.responseObjects[0].contentObjects[0].typeData?? && operation.responseObjects[0].contentObjects[0].typeData == "array">
  ResponseEntity<List<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}>>
  <#else>ResponseEntity<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}>></#if>
  <#else> ResponseEntity<Void>></#if></@compress> ${operation.operationId}WithHttpInfo(<@compress single_line=true>
  <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> ${parameter.className?cap_first} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if>
  <#if operation.requestObjects?has_content>
  <#list operation.requestObjects as request>
  <#list request.contentObjects as content>
  <#if content.typeData?? && (content.typeData == "array")>
  List<${content.refNameObject.refName?cap_first}>
  <#elseif content.typeData?? && (content.typeData == "map")>
  Map<String,${content.refNameObject.refName?cap_first}>
  <#else>
  ${content.refNameObject.refName?cap_first}
  </#if> ${content.refNameObject.refName?uncap_first} <#if content?has_next>, </#if></#list>
  </#list></#if></@compress>) throws WebClientResponseException {
    <#if operation.responseObjects[0].contentObjects[0]??>
    ParameterizedTypeReference<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}> localVarReturnType = new ParameterizedTypeReference<${operation.responseObjects[0].contentObjects[0].refNameObject.refName?cap_first}>() {};
    <#else>
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    </#if>
    return ${operation.operationId}RequestCreation(<@compress single_line=true>
    <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>,</#if></#list> </#if>
    <#if operation.requestObjects?has_content>
    <#list operation.requestObjects as request><#list request.contentObject as content>${content.refNameObject.refName?uncap_first}<#if content?has_next>, </#if></#list></#list></#if>
    </@compress>).<#if operation.responseObjects[0].contentObjects[0].typeData?? && (operation.responseObjects[0].contentObjects[0].typeData == "array" || operation.responseObjects[0].contentObjects[0].typeData == "map")>toEntityList<#else>toEntity</#if>(localVarReturnType);
  }

    </#list>
  </#list>

}
