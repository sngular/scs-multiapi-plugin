package com.sngular.multifileplugin.testIssueFaker;

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

import com.sngular.multifileplugin.testIssueFaker.model.ConfigurationDTO;
import com.sngular.multifileplugin.testIssueFaker.model.FakerSchemaDTO;

public interface FakerApi {

  /**
   * POST /faker/generate-schemas
   * @param configurationDTO (required)
   * @return  OK; (status code 200)  Bad Request; (status code 400)  Not found; (status code 404)
   */

  @Operation(
     operationId = "generateSchema",
     tags = {"SchemaGenerator"},
     responses = {
       @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = FakerSchemaDTO.class))),
       @ApiResponse(responseCode = "400", description = "Bad Request"),
       @ApiResponse(responseCode = "404", description = "Not found")
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/faker/generate-schemas",
    produces = {"application/json"}
  )

  default ResponseEntity<FakerSchemaDTO> generateSchema(@Parameter(name = "configurationDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody ConfigurationDTO configurationDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
