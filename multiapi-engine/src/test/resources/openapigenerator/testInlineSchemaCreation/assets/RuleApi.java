package net.coru.multifileplugin.inlineschemacreation;
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
import javax.validation.Valid;
import java.util.Optional;
import java.util.List;
import java.util.Map;

import net.coru.multifileplugin.inlineschemacreation.model.ApiTestDTO;
import net.coru.multifileplugin.inlineschemacreation.model.ApiErrorDTO;

public interface RuleApi {

  /**
  * GET /rule/{ruleId} 
  * @param ruleId Name of the Rule true
  * @return  OK; (status code 200)  Not Found; (status code 404)  Internal Server Error; (status code 500)
  */

  @Operation(
     operationId = "getSpecificRule",
     tags = { "rules" },
     responses = {
          @ApiResponse(responseCode = "200", description = "OK" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiTestDTO.class)) ), @ApiResponse(responseCode = "404", description = "Not Found" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) ), @ApiResponse(responseCode = "500", description = "Internal Server Error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) ), @ApiResponse(responseCode = "default", description = "Error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/rule/{ruleId}",
    produces = { "application/json" }
  )

  default ResponseEntity<ApiTestDTO> getSpecificRule(@Parameter(name = "ruleId", description = "Name of the Rule", required = true, schema = @Schema(description = "")) @PathVariable("ruleId") Integer ruleId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
  * DELETE /rule/{ruleId} 
  * @param ruleId Name of the Rule true
  * @return  OK; (status code 200)  Not Found; (status code 404)  Internal Server Error; (status code 500)
  */

  @Operation(
     operationId = "deleteRule",
     tags = { "rules" },
     responses = {
          @ApiResponse(responseCode = "200", description = "OK" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = String.class)) ), @ApiResponse(responseCode = "404", description = "Not Found" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) ), @ApiResponse(responseCode = "500", description = "Internal Server Error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) ), @ApiResponse(responseCode = "default", description = "Error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.DELETE,
    value = "/rule/{ruleId}",
    produces = { "application/json" }
  )

  default ResponseEntity<String> deleteRule(@Parameter(name = "ruleId", description = "Name of the Rule", required = true, schema = @Schema(description = "")) @PathVariable("ruleId") Integer ruleId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
  * PUT /rule/{ruleId} 
  * @param ruleId Name of the Rule true
  * @return  OK; (status code 200)  Internal Server Error; (status code 500)  Not Found; (status code 404)
  */

  @Operation(
     operationId = "updateRule",
     tags = { "rules" },
     responses = {
          @ApiResponse(responseCode = "200", description = "OK" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = String.class)) ), @ApiResponse(responseCode = "500", description = "Internal Server Error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) ), @ApiResponse(responseCode = "404", description = "Not Found" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) ), @ApiResponse(responseCode = "default", description = "Error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.PUT,
    value = "/rule/{ruleId}",
    produces = { "application/json" }
  )

  default ResponseEntity<String> updateRule(@Parameter(name = "ruleId", description = "Name of the Rule", required = true, schema = @Schema(description = "")) @PathVariable("ruleId") Integer ruleId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
