package com.sngular.multifileplugin.externalfragmentpathref;

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

import com.sngular.multifileplugin.externalfragmentpathref.model.InlineObjectExampleOperationDTO;
import com.sngular.multifileplugin.externalfragmentpathref.model.InlineResponse200ExampleOperationDTO;

public interface V1Api {

  /**
   * POST /v1/test
   * @param exampleParameter true
   * @param inlineObjectExampleOperationDTO
   * @return  OK; (status code 200)
   */

  @Operation(
    operationId = "exampleOperation",
    tags = {},
    responses = {
      @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InlineResponse200ExampleOperationDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/v1/test",
    produces = {"application/json"}
  )

  default ResponseEntity<InlineResponse200ExampleOperationDTO> exampleOperation(@Parameter(name = "exampleParameter", required = true, schema = @Schema(description = "")) @RequestParam(name = "exampleParameter", required = true) String exampleParameter , @Parameter(name = "inlineObjectExampleOperationDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody(required = false) InlineObjectExampleOperationDTO inlineObjectExampleOperationDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
