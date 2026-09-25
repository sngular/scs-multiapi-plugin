<#ftl output_format="plainText">
<#macro formDataParts request><#list request.contentObjects[0].schemaObject.fieldObjectList as part> @Parameter(name = "${part.baseName}", required = ${part.required?c}, schema = @Schema(description = "")) @RequestPart(value = "${part.baseName}", required = ${part.required?c}) <#if part.dataType.baseType == "multipartfile">FilePart<#elseif part.dataType.baseType == "array" && part.dataType.innerType.baseType == "multipartfile">Flux<FilePart><#else>${part.dataType}</#if> ${part.fieldName}<#if part?has_next>, </#if></#list></#macro>
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
<#assign needsFilePart=false>
<#assign needsResource=false>
<#list pathObjects as path>
  <#list path.operationObjects as operation>
    <#list operation.requestObjects as request>
      <#if request.isFormData>
        <#list request.contentObjects as content>
          <#if content.schemaObject??>
            <#list content.schemaObject.fieldObjectList as field>
              <#if "${field.dataType}"?contains("MultipartFile")>
                <#assign needsFilePart=true>
              </#if>
            </#list>
          </#if>
        </#list>
      </#if>
      <#list request.contentObjects as content>
        <#if content.dataType.containsType("resource")>
          <#assign needsResource=true>
        </#if>
      </#list>
    </#list>
    <#list operation.responseObjects as response>
      <#list response.contentObjects as content>
        <#if content.dataType.containsType("resource")>
          <#assign needsResource=true>
        </#if>
      </#list>
    </#list>
  </#list>
</#list>
<#if needsFilePart>
import org.springframework.http.codec.multipart.FilePart;
</#if>
<#if needsResource>
import org.springframework.core.io.Resource;
</#if>

<#assign imports=[]>
<#assign useDateTimeFormat = false>
<#list pathObjects as path>
  <#list path.operationObjects as operation>
<#list (path.parameterObjects![]) + (operation.parameterObjects![]) as parameter><#if parameter.dateTimeFormatIso??><#assign useDateTimeFormat = true></#if></#list>
    <#list operation.requestObjects as request>
      <#if !request.isFormData>
      <#list request.contentObjects as content>
        <#if content.importName?? && (!imports?seq_contains(content.importName))>
          <#if (!checkBasicTypes?seq_contains(content.importName))>
            <#assign imports = imports+[content.importName]>
          </#if>
        </#if>
      </#list>
      </#if>
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
<#if useDateTimeFormat>
import org.springframework.format.annotation.DateTimeFormat;
</#if>
<#list imports as import>
import <#if packageModel??>${packageModel}<#elseif package??>${package}</#if>.${import};
</#list>

public interface ${className?cap_first}Api {

<#list pathObjects as path>
<#list path.operationObjects as operation>
  /**
   * ${operation.operationType} ${path.pathName}<#if operation.summary?has_content>: ${operation.summary}</#if>
  <#if operation.responseObjects?has_content>
   <#if operation.parameterObjects?has_content>
   * @param <#list operation.parameterObjects as parameter> ${parameter.variableName} ${parameter.description!""} ${parameter.required?c}</#list>
   </#if>
   <#if path.parameterObjects?has_content>
   * @param <#list path.parameterObjects as parameter> ${parameter.variableName} ${parameter.description!""} ${parameter.required?c}</#list>
   </#if>
    <#if operation.requestObjects?has_content>
   *<#list operation.requestObjects as request> @param <#if request.isFormData><#list request.contentObjects[0].schemaObject.fieldObjectList as part>${part.fieldName}<#if part?has_next>, </#if></#list><#else><#list request.contentObjects as content>${content.dataType?api.getVariableNameString()}<#if content?has_next>, </#if></#list></#if>${request.description! ""}<#if request.required == true> (required)</#if></#list>
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
    produces = {"application/json"}<#if operation.requestObjects?has_content && operation.requestObjects[0].isFormData>,
    consumes = {MediaType.MULTIPART_FORM_DATA_VALUE}</#if>
  )
<#if operation.securities?has_content>
  default Mono<ResponseEntity<@compress single_line=true><<#if operation.responseObjects[0].contentObjects[0]??><#if operation.responseObjects[0].contentObjects[0].dataType.baseType == "array" || operation.responseObjects[0].contentObjects[0].dataType.baseType == "map">Flux<${operation.responseObjects[0].contentObjects[0].dataType.innerType}><#else>${operation.responseObjects[0].contentObjects[0].dataType}></#if><#else>Void</#if>></@compress> ${operation.operationId}(<@compress single_line=true>
      <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> @Parameter(name = "${parameter.name}", <#if parameter.description?has_content>description = "${parameter.description!""}", </#if>required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "header"> @RequestHeader(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "cookie"> @CookieValue(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) </#if><#if parameter.dateTimeFormatIso??> @DateTimeFormat(iso = DateTimeFormat.ISO.${parameter.dateTimeFormatIso})</#if> ${parameter.dataType} ${parameter.variableName} <#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter> @Parameter(name = "${parameter.name}", <#if parameter.description?has_content>description = "${parameter.description!""}", </#if>required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "header"> @RequestHeader(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "cookie"> @CookieValue(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) </#if><#if parameter.dateTimeFormatIso??> @DateTimeFormat(iso = DateTimeFormat.ISO.${parameter.dateTimeFormatIso})</#if> ${parameter.dataType} ${parameter.variableName}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if>
      <#if operation.requestObjects?has_content><#list operation.requestObjects as request><#if request.isFormData><@formDataParts request/><#else> @Parameter(name = "${request.contentObjects[0].dataType?api.getVariableNameString()}", description = "${request.description! ""}", required = ${request.required?c}, schema = @Schema(description = "${request.contentObjects[0].description! ""}")) @Valid @RequestBody<#if !request.required>(required = false)</#if>
      <#if request.contentObjects[0].dataType.baseType == "array" || request.contentObjects[0].dataType.baseType == "map"> Flux<${request.contentObjects[0].dataType.innerType}>
      <#else> Mono<${request.contentObjects[0].dataType}> </#if> ${request.contentObjects[0].dataType?api.getVariableNameString()}<#if request?has_next>, </#if></#if></#list></#if></@compress><#if operation.parameterObjects?has_content || operation.requestObjects?has_content>, </#if><#if path.parameterObjects?has_content || path.requestObjects?has_content>, </#if>@ApiIgnore final ServerWebExchange exchange) {
    return Mono.just(new ResponseEntity(HttpStatus.NOT_IMPLEMENTED));
  }
<#else>
  default ResponseEntity<@compress single_line=true><<#if operation.responseObjects[0].contentObjects[0]??><#if operation.responseObjects[0].contentObjects[0].dataType.baseType == "array" || operation.responseObjects[0].contentObjects[0].dataType.baseType == "map">Flux<${operation.responseObjects[0].contentObjects[0].dataType.innerType}><#else>Mono<${operation.responseObjects[0].contentObjects[0].dataType}></#if><#else>Void</#if>></@compress> ${operation.operationId}(<@compress single_line=true>
      <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> @Parameter(name = "${parameter.name}", <#if parameter.description?has_content>description = "${parameter.description!""}", </#if>required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "header"> @RequestHeader(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "cookie"> @CookieValue(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) </#if><#if parameter.dateTimeFormatIso??> @DateTimeFormat(iso = DateTimeFormat.ISO.${parameter.dateTimeFormatIso})</#if> ${parameter.dataType} ${parameter.variableName} <#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if><#if path.parameterObjects?has_content><#list path.parameterObjects as parameter> @Parameter(name = "${parameter.name}", <#if parameter.description?has_content>description = "${parameter.description!""}", </#if>required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "header"> @RequestHeader(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "cookie"> @CookieValue(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) </#if><#if parameter.dateTimeFormatIso??> @DateTimeFormat(iso = DateTimeFormat.ISO.${parameter.dateTimeFormatIso})</#if> ${parameter.dataType} ${parameter.variableName}<#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if>
      <#if operation.requestObjects?has_content><#list operation.requestObjects as request><#if request.isFormData><@formDataParts request/><#else> @Parameter(name = "${request.contentObjects[0].dataType?api.getVariableNameString()}", description = "${request.description! ""}", required = ${request.required?c}, schema = @Schema(description = "${request.contentObjects[0].description! ""}")) @Valid @RequestBody<#if !request.required>(required = false)</#if>
      <#if request.contentObjects[0].dataType.baseType == "array" || request.contentObjects[0].dataType.baseType == "map"> Flux<${request.contentObjects[0].dataType.innerType}>
      <#else> Mono<${request.contentObjects[0].dataType}> </#if> ${request.contentObjects[0].dataType?api.getVariableNameString()}<#if request?has_next>, </#if></#if></#list></#if></@compress><#if operation.parameterObjects?has_content || operation.requestObjects?has_content>, </#if><#if path.parameterObjects?has_content || path.requestObjects?has_content>, </#if>@ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
</#if>

</#list>
</#list>
}