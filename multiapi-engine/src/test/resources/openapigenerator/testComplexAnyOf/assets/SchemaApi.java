package com.sngular.multifileplugin.testcomplexanyof.api;

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

import com.sngular.multifileplugin.testcomplexanyof.model.ApiSchemaDTO;

public interface SchemaApi {

  /**
  * GET /schema/{name} 
  * @param name Schema name to get true
  * @return  OK; (status code 200)  Bad Request; (status code 400)  Not Found; (status code 404)  Server Error; (status code 500)
  */

  @Operation(
     operationId = "getSchema",
     tags = {"schemas"},
     responses = {
       @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ApiSchemaDTO.class))),
       @ApiResponse(responseCode = "400", description = "Bad Request"),
       @ApiResponse(responseCode = "404", description = "Not Found"),
       @ApiResponse(responseCode = "500", description = "Server Error")
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/schema/{name}",
    produces = {"application/json"}
  )

  default ResponseEntity<ApiSchemaDTO> getSchema(@Parameter(name = "name", description = "Schema name to get", required = true, schema = @Schema(description = "")) @PathVariable("name") String name) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
  * PUT /schema/{name} 
  * @param name Schema name to update true
  * @param apiSchemaDTO (required)
  * @return  OK; (status code 200)  Bad Request; (status code 400)  Not Found; (status code 404)  Server Error; (status code 500)
  */

  @Operation(
     operationId = "updateSchema",
     tags = {"schemas"},
     responses = {
       @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ApiSchemaDTO.class))),
       @ApiResponse(responseCode = "400", description = "Bad Request"),
       @ApiResponse(responseCode = "404", description = "Not Found"),
       @ApiResponse(responseCode = "500", description = "Server Error")
     }
  )
  @RequestMapping(
    method = RequestMethod.PUT,
    value = "/schema/{name}",
    produces = {"application/json"}
  )

  default ResponseEntity<ApiSchemaDTO> updateSchema(@Parameter(name = "name", description = "Schema name to update", required = true, schema = @Schema(description = "")) @PathVariable("name") String name , @Parameter(name = "apiSchemaDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody ApiSchemaDTO apiSchemaDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
  * POST /schema 
  * @param apiSchemaDTO (required)
  * @return  OK; (status code 200)  Bad Request; (status code 400)  Server Error; (status code 500)
  */

  @Operation(
     operationId = "createSchema",
     tags = {"schemas"},
     responses = {
       @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ApiSchemaDTO.class))),
       @ApiResponse(responseCode = "400", description = "Bad Request"),
       @ApiResponse(responseCode = "500", description = "Server Error")
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/schema",
    produces = {"application/json"}
  )

  default ResponseEntity<ApiSchemaDTO> createSchema(@Parameter(name = "apiSchemaDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody ApiSchemaDTO apiSchemaDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
  * DELETE /schema/{id} 
  * @param id Subject name to delete true
  * @return  OK; (status code 200)  Bad Request; (status code 400)  Not Found; (status code 404)  Server Error; (status code 500)
  */

  @Operation(
     operationId = "deleteSchema",
     tags = {"schemas"},
     responses = {
       @ApiResponse(responseCode = "200", description = "OK"),
       @ApiResponse(responseCode = "400", description = "Bad Request"),
       @ApiResponse(responseCode = "404", description = "Not Found"),
       @ApiResponse(responseCode = "500", description = "Server Error")
     }
  )
  @RequestMapping(
    method = RequestMethod.DELETE,
    value = "/schema/{id}",
    produces = {"application/json"}
  )

  default ResponseEntity<Void> deleteSchema(@Parameter(name = "id", description = "Subject name to delete", required = true, schema = @Schema(description = "")) @PathVariable("id") String id) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
