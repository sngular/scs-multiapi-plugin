package com.sngular.multifileplugin.testRestrictionsSchema;

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

import com.sngular.multifileplugin.testRestrictionsSchema.model.SchemaDTO;

public interface SchemaMasterApi {

  /**
   * GET /schema-master/{subject}
   * @param subject Retrieve the schema of the Schema Registry, not the Schema Restriction true
   * @return  OK; (status code 200)  Bad Request; (status code 400)  Server Error; (status code 500)
   */

  @Operation(
    operationId = "getSchemaMaster",
    tags = {"SchemaMaster"},
    responses = {
      @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = SchemaDTO.class))),
      @ApiResponse(responseCode = "400", description = "Bad Request"),
      @ApiResponse(responseCode = "500", description = "Server Error")
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/schema-master/{subject}",
    produces = {"application/json"}
  )

  default ResponseEntity<SchemaDTO> getSchemaMaster(@Parameter(name = "subject", description = "Retrieve the schema of the Schema Registry, not the Schema Restriction", required = true, schema = @Schema(description = "")) @PathVariable("subject") String subject) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * POST /schema-master/{schemaType}
   * @param schemaType The schema which you need create in the schema registry true
   * @param schemaDTO (required)
   * @return  OK; (status code 200)  Bad Request; (status code 400)  Server Error; (status code 500)
   */

  @Operation(
    operationId = "createSchemaMaster",
    tags = {"SchemaMaster"},
    responses = {
      @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = SchemaDTO.class))),
      @ApiResponse(responseCode = "400", description = "Bad Request"),
      @ApiResponse(responseCode = "500", description = "Server Error")
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/schema-master/{schemaType}",
    produces = {"application/json"}
  )

  default ResponseEntity<SchemaDTO> createSchemaMaster(@Parameter(name = "schemaType", description = "The schema which you need create in the schema registry", required = true, schema = @Schema(description = "")) @PathVariable("schemaType") String schemaType , @Parameter(name = "schemaDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody SchemaDTO schemaDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
