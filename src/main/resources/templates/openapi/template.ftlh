package <#if packageApi??>${packageApi}<#elseif package??> ${package}</#if>;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.NativeWebRequest;
import java.util.Optional;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.validation.Valid;

<#assign imports=[]>
<#list pathObject as path>
  <#list path.operationObject as operation>
    <#list operation.requestObjects as request>
      <#list request.contentObject as content>
        <#if content.importName?? && (!imports?seq_contains(content.importName))>
        		<#assign imports = imports+[content.importName]>
        </#if>
      </#list>
    </#list>
    <#list operation.responseObjects as response>
      <#list response.contentObject as content>
        <#if content.importName?? && (!imports?seq_contains(content.importName))>
            <#assign imports = imports+[content.importName]>
        </#if>
      </#list>
    </#list>
  </#list>
</#list>
<#list imports as import>
import <#if packageModel??>${packageModel}<#elseif package??>${package}</#if>.${import};
</#list>


public interface ${className?cap_first}Api {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    <#list pathObject as path>
       <#list path.operationObject as operation>
         /**
         * ${operation.operationType} ${path.pathName} : ${operation.summary} <#if operation.responseObjects?has_content>
         *<#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> @param ${parameter.name} ${parameter.description} ${parameter.required?c} </#list></#if>
         <#if operation.requestObjects?has_content>*<#list operation.requestObjects as request> <#list request.contentObject as content>@param ${content.refName?uncap_first}<#if content?has_next>, </#if></#list>${request.description! ""}<#if request.required == true> (required)</#if></#list></#if>
         * @return<#list operation.responseObjects as response><#if response.responseName != "default">  ${response.description}; (status code ${response.responseName}) </#if>
         </#list></#if>*/

         @Operation(
            operationId = "${operation.operationId}",
            summary = "${operation.summary}",
            tags = { <#list operation.tags as tags>"${tags}"</#list> }<#if operation.responseObjects?has_content>,
            responses = {
                <#list operation.responseObjects as response> @ApiResponse(responseCode = "${response.responseName}", description = "${response.description}" <#if response.contentObject?has_content>, content = @Content(<#list response.contentObject as content>mediaType = "${content.name}" <#if content.refName?has_content>,schema = @Schema(implementation = ${content.refName}.class))</#if> </#list></#if>)<#if !response?is_last>,</#if></#list>
            }</#if>
         )
         @RequestMapping(
           method = RequestMethod.${operation.operationType},
           value = "${path.pathName}",
           produces = { "application/json" }
         )


         default ResponseEntity<#compress><<#if operation.responseObjects[0].contentObject[0]??><#if operation.responseObjects[0].contentObject[0].typeData?? && (operation.responseObjects[0].contentObject[0].typeData == "array")>
                ArrayList<${operation.responseObjects[0].contentObject[0].refName?cap_first}>
                <#elseif operation.responseObjects[0].contentObject[0].typeData?? && (operation.responseObjects[0].contentObject[0].typeData == "map")>
                HashMap< String,${operation.responseObjects[0].contentObject[0].refName?cap_first}>
                <#else>${operation.responseObjects[0].contentObject[0].refName?cap_first}</#if><#else>Void</#if>></#compress> ${operation.operationId} (<#compress>
                <#if operation.parameterObjects?has_content><#list operation.parameterObjects as parameter> @Parameter(name = "${parameter.name}", description = "${parameter.description}", required = ${parameter.required?c}, schema = @Schema(description = "")) <#if parameter.in == "path"> @PathVariable("${parameter.name}") <#elseif parameter.in == "query"> @RequestParam </#if> ${parameter.className?cap_first} ${parameter.name} <#if parameter?has_next || operation.requestObjects?has_content> , </#if></#list></#if>
                <#if operation.requestObjects?has_content><#list operation.requestObjects as request> @Parameter(name = "${request.contentObject[0].refName}", description = "${request.description! ""}", required = ${request.required?c} , schema = @Schema(description = "${request.contentObject[0].description! ""}")) @Valid @RequestBody
                <#if request.contentObject[0].typeData?? && (request.contentObject[0].typeData == "array")> ArrayList<${request.contentObject[0].refName?cap_first}>
                <#elseif request.contentObject[0].typeData?? && (request.contentObject[0].typeData == "map")> HashMap< String,${request.contentObject[0].refName?cap_first}>
                <#else> ${request.contentObject[0].refName?cap_first} </#if> ${request.contentObject[0].refName?uncap_first} <#if request?has_next> , </#if></#list></#if></#compress>) {
                return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
          }
       </#list>
    </#list>



}
