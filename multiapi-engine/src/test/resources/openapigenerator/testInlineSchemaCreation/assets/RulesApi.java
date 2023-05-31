package com.sngular.multifileplugin.inlineschemacreation;

import java.util.Optional;
import java.util.List;
import java.util.Map;
import javax.validation.Valid;

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

import com.sngular.multifileplugin.inlineschemacreation.model.ApiTestDTO;
import com.sngular.multifileplugin.inlineschemacreation.model.ApiErrorDTO;

public interface RulesApi {

  /**
   * GET /rules
   * @return  OK; (status code 200)  Internal Server Error; (status code 500)
   */

  @Operation(
     operationId = "getAllRules",
     tags = {"rules"},
     responses = {
       @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))),
       @ApiResponse(responseCode = "500", description = "Internal Server Error", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ApiErrorDTO.class))),
       @ApiResponse(responseCode = "default", description = "Error", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ApiErrorDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/rules",
    produces = {"application/json"}
  )

  default ResponseEntity<List<ApiTestDTO>> getAllRules() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * POST /rules
   * @param apiTestDTO (required)
   * @return  OK; (status code 200)  Internal Server Error; (status code 500)
   */

  @Operation(
     operationId = "createRule",
     tags = {"rules"},
     responses = {
       @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ApiTestDTO.class))),
       @ApiResponse(responseCode = "500", description = "Internal Server Error", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ApiErrorDTO.class))),
       @ApiResponse(responseCode = "default", description = "Error", content = @Content(mediaType = "text/plain", schema = @Schema(implementation = String.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/rules",
    produces = {"application/json"}
  )

  default ResponseEntity<ApiTestDTO> createRule(@Parameter(name = "apiTestDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody ApiTestDTO apiTestDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
