<#ftl output_format="plainText">
<#function streamsRequest operation><#return operation.requestObjects?has_content && operation.requestObjects[0].streaming></#function>
<#function isStreaming operation><#return operation.streamingResponse || streamsRequest(operation)></#function>
<#macro boundParameter parameter><@compress single_line=true>@Parameter(name = "${parameter.name}", <#if parameter.description?has_content>description = "${parameter.description}", </#if>required = ${parameter.required?c}, schema = @Schema(description = ""))
  <#if parameter.in == "path">@PathVariable("${parameter.name}")<#elseif parameter.in == "query">@RequestParam(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>)<#elseif parameter.in == "header">@RequestHeader(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>)<#elseif parameter.in == "cookie">@CookieValue(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>)</#if><#if parameter.dateTimeFormatIso??> @DateTimeFormat(iso = DateTimeFormat.ISO.${parameter.dateTimeFormatIso})</#if> ${parameter.dataType} ${parameter.variableName}</@compress></#macro>
<#-- The arguments of the endpoint of a streaming operation: its bound parameters and body, and the request it reads or streams through. -->
<#function streamingEndpointArgs operation path withRequest withResponse>
  <#local args = []>
  <#list (operation.parameterObjects![]) + (path.parameterObjects![]) as parameter><#local bound><@boundParameter parameter/></#local><#local args = args + [bound]></#list>
  <#list operation.requestObjects as request>
    <#if request.isFormData>
      <#list request.contentObjects[0].schemaObject.fieldObjectList as part><#local args = args + ["@Parameter(name = \"${part.baseName}\", required = ${part.required?c}, schema = @Schema(description = \"\")) @RequestPart(value = \"${part.baseName}\", required = ${part.required?c}) ${part.dataType} ${part.fieldName}"]></#list>
    <#elseif !request.streaming>
      <#local args = args + ["@Parameter(name = \"${request.contentObjects[0].dataType?api.getVariableNameString()}\", description = \"${request.description! \"\"}\", required = ${request.required?c}, schema = @Schema(description = \"${request.contentObjects[0].description! \"\"}\")) @Valid @RequestBody" + request.required?then("", "(required = false)") + " ${request.contentObjects[0].dataType} ${request.contentObjects[0].dataType?api.getVariableNameString()}"]>
    </#if>
  </#list>
  <#if withRequest><#local args = args + ["@Parameter(hidden = true) final HttpServletRequest servletRequest"]></#if>
  <#if withResponse><#local args = args + ["@Parameter(hidden = true) final HttpServletResponse servletResponse"]></#if>
  <#return args?join(", ")>
</#function>
<#-- The parameters of the method a streaming operation is implemented in: the endpoint's, unbound, with the streamed body as a Stream of its items. -->
<#function streamingMethodParams operation path>
  <#local params = []>
  <#list (operation.parameterObjects![]) + (path.parameterObjects![]) as parameter><#local params = params + ["final ${parameter.dataType} ${parameter.variableName}"]></#list>
  <#list operation.requestObjects as request>
    <#if request.isFormData>
      <#list request.contentObjects[0].schemaObject.fieldObjectList as part><#local params = params + ["final ${part.dataType} ${part.fieldName}"]></#list>
    <#elseif request.streaming>
      <#local params = params + ["final Stream<${request.contentObjects[0].dataType}> ${request.contentObjects[0].dataType?api.getVariableNameString()}"]>
    <#else>
      <#local params = params + ["final ${request.contentObjects[0].dataType} ${request.contentObjects[0].dataType?api.getVariableNameString()}"]>
    </#if>
  </#list>
  <#return params?join(", ")>
</#function>
<#function streamingMethodCall operation path>
  <#local args = []>
  <#list (operation.parameterObjects![]) + (path.parameterObjects![]) as parameter><#local args = args + [parameter.variableName]></#list>
  <#list operation.requestObjects as request>
    <#if request.isFormData>
      <#list request.contentObjects[0].schemaObject.fieldObjectList as part><#local args = args + [part.fieldName]></#list>
    <#elseif request.streaming>
      <#local args = args + ["NdjsonSupport.read(servletRequest, new TypeReference<${request.contentObjects[0].dataType}>() {})"]>
    <#else>
      <#local args = args + [request.contentObjects[0].dataType?api.getVariableNameString()]>
    </#if>
  </#list>
  <#return operation.operationId + "(" + args?join(", ") + ")">
</#function>
<#macro operationAnnotation path operation>
  @Operation(
    operationId = "${operation.operationId}",<#if operation.summary?has_content>
    summary = "${operation.summary}",</#if>
    tags = {<#list operation.tags as tag>"${tag}"</#list>}<#if operation.responseObjects?has_content>,
    responses = {
<#list operation.responseObjects as response>      @ApiResponse(responseCode = "${response.responseName}", description = "${response.description}"<#if response.contentObjects?has_content>, content = <#if (response.contentObjects?size > 1)>{</#if><#list response.contentObjects as content>@Content(mediaType = "${content.name}", schema = @Schema(implementation = ${content.dataType?api.getClassString()}))<#sep>, </#list><#if (response.contentObjects?size > 1)>}</#if></#if>)<#if !response?is_last>,</#if>
</#list>
    }</#if>
  )
</#macro>
<#-- A streaming operation: an endpoint per media type, which binds the request and sends the response, around the method the operation is implemented in. -->
<#macro streamingOperation path operation>
<#assign streamedType = operation.streamingResponse?then("Stream<${operation.streamingItemType}>", (operation.responseObjects[0].contentObjects[0]??)?then("${operation.responseObjects[0].contentObjects[0].dataType}", "Void"))>
<#assign consumesStream = streamsRequest(operation)?then(",\n    consumes = {\"${operation.consumes[0]}\"}", (operation.requestObjects?has_content && operation.requestObjects[0].isFormData)?then(",\n    consumes = {MediaType.MULTIPART_FORM_DATA_VALUE}", ""))>
<#assign throwsIo = streamsRequest(operation)?then(" throws IOException", "")>
<#if operation.streamingResponse>
  /**
   * ${operation.operationType} ${path.pathName}<#if operation.summary?has_content>: ${operation.summary}</#if>, sending the items {@link #${operation.operationId}} returns as ${operation.streamingMediaType}, one JSON document per line.
   */
<@operationAnnotation path operation/>
  @RequestMapping(
    method = RequestMethod.${operation.operationType},
    value = "${path.pathName}",
    produces = {"${operation.streamingMediaType}"}${consumesStream}
  )
  default ResponseEntity<StreamingResponseBody> ${operation.operationId}Ndjson(${streamingEndpointArgs(operation, path, true, true)})${throwsIo} {
    return NdjsonSupport.stream(${streamingMethodCall(operation, path)}, "${operation.streamingMediaType}", servletRequest, servletResponse);
  }

  <#if operation.streamingAlsoJson>
  /**
   * ${operation.operationType} ${path.pathName}<#if operation.summary?has_content>: ${operation.summary}</#if>, sending the items {@link #${operation.operationId}} returns as a JSON array.
   */
<@operationAnnotation path operation/>
  @RequestMapping(
    method = RequestMethod.${operation.operationType},
    value = "${path.pathName}",
    produces = {"application/json"}${consumesStream}
  )
  default ResponseEntity<List<${operation.streamingItemType}>> ${operation.operationId}Json(${streamingEndpointArgs(operation, path, streamsRequest(operation), false)})${throwsIo} {
    return NdjsonSupport.collect(${streamingMethodCall(operation, path)});
  }

  </#if>
<#else>
  /**
   * ${operation.operationType} ${path.pathName}<#if operation.summary?has_content>: ${operation.summary}</#if>, reading the ${operation.consumes[0]} body, one JSON document per line, as the items {@link #${operation.operationId}} receives.
   */
<@operationAnnotation path operation/>
  @RequestMapping(
    method = RequestMethod.${operation.operationType},
    value = "${path.pathName}",
    produces = {"application/json"}${consumesStream}
  )
  default ResponseEntity<${streamedType}> ${operation.operationId}Ndjson(${streamingEndpointArgs(operation, path, true, false)})${throwsIo} {
    return ${streamingMethodCall(operation, path)};
  }

</#if>
  /**
   * ${operation.operationType} ${path.pathName}<#if operation.summary?has_content>: ${operation.summary}</#if>. Implement this method rather than the endpoint<#if operation.streamingAlsoJson>s</#if> above<#if operation.streamingResponse>: the items are sent as they are produced, and the stream is closed once they are sent</#if><#if streamsRequest(operation)><#if operation.streamingResponse>;<#else>:</#if> the body items are read as the stream is consumed</#if>.
   */
  default ResponseEntity<${streamedType}> ${operation.operationId}(${streamingMethodParams(operation, path)}) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

</#macro>
package <#if packageApi??>${packageApi}<#elseif package??> ${package}</#if>;

<#assign imports=[]>
<#assign model_imports=[]>
<#assign needsMultipartFile=false>
<#assign needsResource=false>
<#list pathObjects as path>
    <#list path.operationObjects as operation>
        <#list operation.requestObjects as request>
            <#if request.isFormData>
                <#list request.contentObjects as content>
                    <#if content.schemaObject??>
                        <#list content.schemaObject.fieldObjectList as field>
                            <#if "${field.dataType}"?contains("MultipartFile")>
                                <#assign needsMultipartFile=true>
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
<#list pathObjects as path>
    <#list path.operationObjects as operation>
<#list (path.parameterObjects![]) + (operation.parameterObjects![]) as parameter><#if parameter.dateTimeFormatIso?? && !imports?seq_contains("org.springframework.format.annotation.DateTimeFormat")><#assign imports = imports + ["org.springframework.format.annotation.DateTimeFormat"]></#if></#list>
        <#list operation.requestObjects as request>
            <#if !request.isFormData>
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
            </#if>
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
import java.util.Optional;
import java.util.List;
import java.util.Map;
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
<#if needsMultipartFile>
import org.springframework.web.multipart.MultipartFile;
</#if>
<#if needsResource>
import org.springframework.core.io.Resource;
</#if>
<#assign streamingResponse=false>
<#assign streamingRequest=false>
<#list pathObjects as path><#list path.operationObjects as operation><#if operation.streamingResponse><#assign streamingResponse=true></#if><#if streamsRequest(operation)><#assign streamingRequest=true></#if></#list></#list>
<#if streamingResponse || streamingRequest>
import java.util.stream.Stream;
import ${javaEEPackage}.servlet.http.HttpServletRequest;
</#if>
<#if streamingResponse>
import ${javaEEPackage}.servlet.http.HttpServletResponse;
</#if>
<#if streamingRequest>
import java.io.IOException;
import ${jacksonPackage}.core.type.TypeReference;
</#if>
<#if streamingResponse>
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;
</#if>

<#list model_imports as import>
import <#if packageModel??>${packageModel}<#elseif package??>${package}</#if>.${import};
</#list>

public interface ${className?cap_first}Api {

<#list pathObjects as path>
<#list path.operationObjects as operation>
<#if isStreaming(operation)>
<@streamingOperation path operation/>
<#else>
  /**
   * ${operation.operationType} ${path.pathName}<#if operation.summary?has_content>: ${operation.summary}</#if><#if operation.responseObjects?has_content>
   <#if operation.parameterObjects?has_content>
   *<#list operation.parameterObjects as parameter> @param ${parameter.variableName} <#if parameter.description?has_content>${parameter.description} </#if>${parameter.required?c}</#list>
   </#if>
   <#if path.parameterObjects?has_content>
   *<#list path.parameterObjects as parameter> @param ${parameter.variableName} <#if parameter.description?has_content>${parameter.description} </#if>${parameter.required?c}</#list>
   </#if>
   <#if operation.requestObjects?has_content>
   *<#list operation.requestObjects as request><#if request.isFormData><#list request.contentObjects[0].schemaObject.fieldObjectList as part> @param ${part.fieldName}</#list><#else><#list request.contentObjects as content> @param ${content.dataType?api.getVariableNameString()}<#if content?has_next>, </#if></#list></#if>${request.description! ""}<#if request.required == true> (required)</#if></#list>
   </#if>
   * @return<#list operation.responseObjects as response><#if response.responseName != "default">  ${response.description}; (status code ${response.responseName})</#if></#list></#if>
   */

  @Operation(
    operationId = "${operation.operationId}",<#if operation.summary?has_content>
    summary = "${operation.summary}",</#if>
    tags = {<#list operation.tags as tag>"${tag}"</#list>}<#if operation.responseObjects?has_content>,
    responses = {
<#list operation.responseObjects as response>      @ApiResponse(responseCode = "${response.responseName}", description = "${response.description}"<#if response.contentObjects?has_content>, content = <#if (response.contentObjects?size > 1)>{</#if><#list response.contentObjects as content>@Content(mediaType = "${content.name}", schema = @Schema(implementation = ${content.dataType?api.getClassString()}))<#sep>, </#list><#if (response.contentObjects?size > 1)>}</#if></#if>)<#if !response?is_last>,</#if>
       </#list>
    }</#if>
  )
  @RequestMapping(
    method = RequestMethod.${operation.operationType},
    value = "${path.pathName}",
    produces = {"application/json"}<#if operation.requestObjects?has_content && operation.requestObjects[0].isFormData>,
    consumes = {MediaType.MULTIPART_FORM_DATA_VALUE}</#if>
  )

  default ResponseEntity<@compress single_line=true><#if operation.responseObjects[0].contentObjects[0]??>
    <${operation.responseObjects[0].contentObjects[0].dataType}<#else><Void</#if>></@compress> ${operation.operationId}(<@compress single_line=true>
    <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> @Parameter(name = "${parameter.name}", <#if parameter.description?has_content>description = "${parameter.description}", </#if>required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "header"> @RequestHeader(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "cookie"> @CookieValue(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) </#if><#if parameter.dateTimeFormatIso??> @DateTimeFormat(iso = DateTimeFormat.ISO.${parameter.dateTimeFormatIso})</#if> ${parameter.dataType} ${parameter.variableName} <#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if>
    <#if path.parameterObjects?has_content><#list path.parameterObjects as parameter> @Parameter(name = "${parameter.name}", <#if parameter.description?has_content>description = "${parameter.description}", </#if>required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "header"> @RequestHeader(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) <#elseif parameter.in == "cookie"> @CookieValue(name = "${parameter.name}", required = ${parameter.required?c}<#if parameter.defaultValue??>, defaultValue = "${parameter.defaultValue?j_string}"</#if>) </#if><#if parameter.dateTimeFormatIso??> @DateTimeFormat(iso = DateTimeFormat.ISO.${parameter.dateTimeFormatIso})</#if> ${parameter.dataType} ${parameter.variableName} <#if parameter?has_next || operation.requestObjects?has_content>, </#if></#list></#if>
    <#if operation.requestObjects?has_content><#list operation.requestObjects as request><#if request.isFormData><#list request.contentObjects[0].schemaObject.fieldObjectList as part> @Parameter(name = "${part.baseName}", required = ${part.required?c}, schema = @Schema(description = "")) @RequestPart(value = "${part.baseName}", required = ${part.required?c}) ${part.dataType} ${part.fieldName} <#if part?has_next || request?has_next>, </#if></#list><#else> @Parameter(name = "${request.contentObjects[0].dataType?api.getVariableNameString()}", description = "${request.description! ""}", required = ${request.required?c}, schema = @Schema(description = "${request.contentObjects[0].description! ""}")) @Valid @RequestBody<#if !request.required>(required = false)</#if>
    ${request.contentObjects[0].dataType} ${request.contentObjects[0].dataType?api.getVariableNameString()} <#if request?has_next>, </#if></#if></#list></#if></@compress>) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
</#if>
  </#list>
</#list>

}
