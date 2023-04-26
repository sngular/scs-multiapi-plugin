package com.sngular.multifileplugin.testListString;

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


public interface TestInlineApi {

  /**
  * GET /test-inline 
  * @return  A list of strings; (status code 200)
  */

  @Operation(
     operationId = "testListStringInline",
     tags = {"test-inline"},
     responses = {
       @ApiResponse(responseCode = "200", description = "A list of strings", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test-inline",
    produces = {"application/json"}
  )

  default ResponseEntity<List<String>> testListStringInline() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
