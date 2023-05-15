<#ftl output_format="plainText">
package <#if packageApi??>${packageApi}<#elseif package??>${package}</#if>;

import java.util.List;
import java.util.Map;
import java.nio.charset.StandardCharsets;
import ${javaEEPackage}.validation.Valid;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.http.MediaType;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import springfox.documentation.annotations.ApiIgnore;

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
    <#list operation.parameterObjects as parameter>
        <#if parameter.importName?? && (!imports?seq_contains(parameter.importName))>
          <#if (!checkBasicTypes?seq_contains(parameter.importName))>
            <#assign imports = imports+[parameter.importName]>
          </#if>
        </#if>
    </#list>
  </#list>
</#list>
<#list imports as import>
import <#if packageModel??>${packageModel}<#elseif package??>${package}</#if>.${import};
</#list>

public interface ${className?cap_first}Api {

<#list pathObjects as path>
<#list path.operationObjects as operation>
  /**
  * ${operation.operationType} ${path.pathName} <#if operation.summary?has_content>: ${operation.summary}</#if>
  <#if operation.responseObjects?has_content>
   <#if operation.parameterObjects?has_content>
  * @param <#list operation.parameterObjects as parameter> ${parameter.name} ${parameter.description} ${parameter.required?c}</#list>
   </#if>
   <#if path.parameterObjects?has_content>
  * @param <#list path.parameterObjects as parameter> ${parameter.name} ${parameter.description} ${parameter.required?c}</#list>
   </#if>
    <#if operation.requestObjects?has_content>
  *<#list operation.requestObjects as request> @param <#list request.contentObjects as content>${content.dataType?api.getVariableNameString()}<#if content?has_next>, </#if></#list>${request.description! ""}<#if request.required == true> (required)</#if></#list>
   </#if>
  * @return<#list operation.responseObjects as response><#if response.responseName != "default">  ${response.description}; (status code ${response.responseName})</#if></#list>
  </#if>
  * @throws WebClientResponseException if an error occurs while attempting to invoke the API
  */
  @Operation(
     operationId = "${operation.operationId}",<#if operation.summary?has_content>
     summary = "${operation.summary}",</#if>
     tags = {<#list operation.tags as tag>"${tag}"</#list>}<#if operation.responseObjects?has_content>,
     responses = {
<#list operation.responseObjects as response>       @ApiResponse(responseCode = "${response.responseName}", description = "${response.description}"<#if response.contentObjects?has_content>, content = @Content(<#list response.contentObjects as content>mediaType = "${content.name}", schema = @Schema(implementation = <#if content.dataType.baseType == "array" || content.dataType.baseType == "map">${content.dataType.innerType?api.getClassString()}<#else>${content.dataType?api.getClassString()}</#if>))</#list></#if>)<#if !response?is_last>,</#if>
</#list>
     }</#if>
  )
  @RequestMapping(
    method = RequestMethod.${operation.operationType},
    value = "${path.pathName}",
    produces = {"application/json"}
  )
<#if operation.securities?has_content>
  default Mono<ResponseEntity<@compress single_line=true><<#if operation.responseObjects[0].contentObjects[0]??><#if operation.responseObjects[0].contentObjects[0].dataType.baseType == "array" || operation.responseObjects[0].contentObjects[0].dataType.baseType == "map">Flux<${operation.responseObjects[0].contentObjects[0].dataType.innerType}><#else>${operation.responseObjects[0].contentObjects[0].dataType}></#if><#else>Void</#if>></@compress> ${operation.operationId}(<@compress single_line=true>
      <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> @Parameter(name = "${parameter.name}", description = "${parameter.description}", required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(required = ${parameter.required?c}) </#if> ${parameter.dataType} ${parameter.name} <#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter> @Parameter(name = "${parameter.name}", description = "${parameter.description}", required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(required = ${parameter.required?c}) </#if> ${parameter.dataType} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if>
      <#if operation.requestObjects?has_content><#list operation.requestObjects as request> @Parameter(name = "${request.contentObjects[0].dataType?api.getVariableNameString()}", description = "${request.description! ""}", required = ${request.required?c}, schema = @Schema(description = "${request.contentObjects[0].description! ""}")) @Valid @RequestBody
      <#if request.contentObjects[0].dataType.baseType == "array" || request.contentObjects[0].dataType.baseType == "map"> Flux<${request.contentObjects[0].dataType.innerType}>
      <#else> Mono<${request.contentObjects[0].dataType}> </#if> ${request.contentObjects[0].dataType?api.getVariableNameString()}<#if request?has_next>, </#if></#list></#if></@compress><#if operation.parameterObjects?has_content || operation.requestObjects?has_content>, </#if><#if path.parameterObjects?has_content || path.requestObjects?has_content>, </#if>@ApiIgnore final ServerWebExchange exchange) {
    return Mono.just(new ResponseEntity(HttpStatus.NOT_IMPLEMENTED));
  }
<#else>
  default ResponseEntity<@compress single_line=true><<#if operation.responseObjects[0].contentObjects[0]??><#if operation.responseObjects[0].contentObjects[0].dataType.baseType == "array" || operation.responseObjects[0].contentObjects[0].dataType.baseType == "map">Flux<${operation.responseObjects[0].contentObjects[0].dataType.innerType}><#else>Mono<${operation.responseObjects[0].contentObjects[0].dataType}></#if><#else>Void</#if>></@compress> ${operation.operationId}(<@compress single_line=true>
      <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> @Parameter(name = "${parameter.name}", description = "${parameter.description}", required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(required = ${parameter.required?c}) </#if> ${parameter.dataType} ${parameter.name} <#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter> @Parameter(name = "${parameter.name}", description = "${parameter.description}", required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(required = ${parameter.required?c}) </#if> ${parameter.dataType} ${parameter.name}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if>
      <#if operation.requestObjects?has_content><#list operation.requestObjects as request> @Parameter(name = "${request.contentObjects[0].dataType?api.getVariableNameString()}", description = "${request.description! ""}", required = ${request.required?c}, schema = @Schema(description = "${request.contentObjects[0].description! ""}")) @Valid @RequestBody
      <#if request.contentObjects[0].dataType.baseType == "array" || request.contentObjects[0].dataType.baseType == "map"> Flux<${request.contentObjects[0].dataType.innerType}>
      <#else> Mono<${request.contentObjects[0].dataType}> </#if> ${request.contentObjects[0].dataType?api.getVariableNameString()}<#if request?has_next>, </#if></#list></#if></@compress><#if operation.parameterObjects?has_content || operation.requestObjects?has_content>, </#if><#if path.parameterObjects?has_content || path.requestObjects?has_content>, </#if>@ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
</#if>

</#list>
</#list>
}