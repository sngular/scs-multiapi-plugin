package com.sngular.multifileplugin.testQueryParam;

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


public interface TestApi {

  /**
   * GET /test
   * @param param1 First test parameter false @param param2 Second test parameter true
   * @return  An object with a list of strings; (status code 200)
   */

  @Operation(
    operationId = "testReactiveQueryParam",
    tags = {"test"},
    responses = {
      @ApiResponse(responseCode = "200", description = "An object with a list of strings", content = @Content(mediaType = "application/json", schema = @Schema(implementation = String.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test",
    produces = {"application/json"}
  )

  default ResponseEntity<String> testReactiveQueryParam(@Parameter(name = "param1", description = "First test parameter", required = false, schema = @Schema(description = "")) @RequestParam(required = false) String param1 , @Parameter(name = "param2", description = "Second test parameter", required = true, schema = @Schema(description = "")) @RequestParam(required = true) String param2) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
