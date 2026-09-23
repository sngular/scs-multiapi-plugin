<#ftl output_format="plainText">
package <#if packageApi??>${packageApi}<#elseif package??> ${package}</#if>;

<#assign imports=[]>
<#assign model_imports=[]>
<#list pathObjects as path>
  <#list path.operationObjects as operation>
    <#list operation.requestObjects?filter(request -> !request.isFormData) as request>
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
<#-- Spring HTTP service interface: the consuming service backs it with its own RestClient/WebClient through
     HttpServiceProxyFactory (or @ImportHttpServices on Spring Boot 4), so paths are relative and no HTTP plumbing,
     authentication or server URL is generated. -->
<#function exchangeAnnotation method>
  <#switch method?upper_case>
    <#case "GET"><#return "GetExchange">
    <#case "POST"><#return "PostExchange">
    <#case "PUT"><#return "PutExchange">
    <#case "PATCH"><#return "PatchExchange">
    <#case "DELETE"><#return "DeleteExchange">
    <#default><#return "HttpExchange">
  </#switch>
</#function>
<#function returnType operation>
  <#if operation.responseObjects?has_content && operation.responseObjects[0].contentObjects[0]??>
    <#assign entity = "ResponseEntity<" + operation.responseObjects[0].contentObjects[0].dataType + ">">
  <#else>
    <#assign entity = "ResponseEntity<Void>">
  </#if>
  <#return isReactive?then("Mono<" + entity + ">", entity)>
</#function>
<#macro parameterBinding parameter><@compress single_line=true>
  <#if parameter.in == "path">@PathVariable("${parameter.name}")
  <#elseif parameter.in == "query">@RequestParam(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>)
  <#elseif parameter.in == "header">@RequestHeader(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>)
  <#elseif parameter.in == "cookie">@CookieValue(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>)
  </#if><#if parameter.dateTimeFormatIso??> @DateTimeFormat(iso = DateTimeFormat.ISO.${parameter.dateTimeFormatIso})</#if> ${parameter.dataType} ${parameter.variableName}
</@compress></#macro>
<#assign useDateTimeFormat = false>
<#list pathObjects as path><#list path.operationObjects as operation><#list (path.parameterObjects![]) + (operation.parameterObjects![]) as parameter><#if parameter.dateTimeFormatIso??><#assign useDateTimeFormat = true></#if></#list></#list></#list>
<#assign useMultipart = false>
<#list pathObjects as path><#list path.operationObjects as operation><#list operation.requestObjects as request><#if request.isFormData><#assign useMultipart = true></#if></#list></#list></#list>
import java.util.List;

import org.springframework.http.ResponseEntity;
<#if useMultipart>
import org.springframework.web.multipart.MultipartFile;
</#if>
<#if useDateTimeFormat>
import org.springframework.format.annotation.DateTimeFormat;
</#if>
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PatchExchange;
import org.springframework.web.service.annotation.PostExchange;
import org.springframework.web.service.annotation.PutExchange;
<#if isReactive>
import reactor.core.publisher.Mono;
</#if>

<#list model_imports as import>
import <#if packageModel??>${packageModel}<#elseif package??>${package}</#if>.${import};
</#list>

/**
 * ${className?cap_first} API, as a Spring HTTP service interface. Back it with a configured {@code <#if isReactive>WebClient<#else>RestClient</#if>}, whose
 * base URL, authentication, timeouts and message converters apply to every request:
 * <pre>
 * HttpServiceProxyFactory.builderFor(<#if isReactive>WebClientAdapter.create(webClient)<#else>RestClientAdapter.create(restClient)</#if>).build()
 *     .createClient(${className?cap_first}Api.class);
 * </pre>
 * or, on Spring Boot 4, register it with {@code @ImportHttpServices}.
 */
@HttpExchange
public interface ${className?cap_first}Api {
<#list pathObjects as path>
  <#list path.operationObjects as operation>

  /**
   * ${operation.operationType} ${path.pathName}<#if operation.summary?has_content>: ${operation.summary}</#if>
  <#list (operation.parameterObjects![]) + (path.parameterObjects![]) as parameter>
   * @param ${parameter.variableName} ${parameter.description!""}<#if parameter.required> (required)</#if>
  </#list>
  <#list operation.requestObjects as request>
    <#if request.isFormData>
      <#list request.contentObjects[0].schemaObject.fieldObjectList as part>
   * @param ${part.fieldName} multipart part "${part.baseName}"<#if part.required> (required)</#if>
      </#list>
    <#else>
   * @param ${request.contentObjects[0].dataType?api.getVariableNameString()} ${request.description!""}<#if request.required> (required)</#if>
    </#if>
  </#list>
   * @return<#list operation.responseObjects as response><#if response.responseName != "default"> ${response.description!""} (status code ${response.responseName});</#if></#list>
   */
  @${exchangeAnnotation(operation.operationType)}(<#if exchangeAnnotation(operation.operationType) == "HttpExchange">method = "${operation.operationType?upper_case}", </#if>url = "${path.pathName}"<#if operation.produces?has_content>, accept = {<#list operation.produces as produce>"${produce}"<#sep>, </#list>}</#if><#if operation.requestObjects?has_content && operation.consumes?has_content>, contentType = "${operation.consumes[0]}"</#if>)
  ${returnType(operation)} ${operation.operationId}(<#list (operation.parameterObjects![]) + (path.parameterObjects![]) as parameter><@parameterBinding parameter/><#sep>, </#list><#if operation.requestObjects?has_content><#if (operation.parameterObjects![])?has_content || (path.parameterObjects![])?has_content>, </#if><#list operation.requestObjects as request><#if request.isFormData><#list request.contentObjects[0].schemaObject.fieldObjectList as part>@RequestPart(name = "${part.baseName}", required = ${part.required?c}) ${part.dataType} ${part.fieldName}<#sep>, </#list><#else>@RequestBody(required = ${request.required?c}) ${request.contentObjects[0].dataType} ${request.contentObjects[0].dataType?api.getVariableNameString()}</#if><#sep>, </#list></#if>);
  </#list>
</#list>
}
