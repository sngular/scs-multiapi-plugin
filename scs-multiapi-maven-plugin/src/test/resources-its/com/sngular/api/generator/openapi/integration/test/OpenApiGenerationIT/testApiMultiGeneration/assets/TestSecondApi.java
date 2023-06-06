package com.sngular.multifileplugin.testmultifile.second;

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

import com.sngular.multifileplugin.testmultifile.second.model.TestSecondDTO;
import com.sngular.multifileplugin.testmultifile.second.model.ErrorSecondDTO;
import com.sngular.multifileplugin.testmultifile.second.model.TestInfoSecondDTO;

public interface TestSecondApi {

  /**
   * GET /testSecond: List all available test
   * @return  A paged array of tests; (status code 200)
   */

  @Operation(
     operationId = "listTest",
     summary = "List all available test",
     tags = {"testSecond"},
     responses = {
       @ApiResponse(responseCode = "200", description = "A paged array of tests", content = @Content(mediaType = "application/json", schema = @Schema(implementation = TestSecondDTO.class))),
       @ApiResponse(responseCode = "default", description = "unexpected error", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorSecondDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/testSecond",
    produces = {"application/json"}
  )

  default ResponseEntity<TestSecondDTO> listTest() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * GET /testSecond/{testId}: Info for a specific test
   * @param testId The id of the test to retrieve true
   * @return  Expected response to a valid request; (status code 200)
   */

  @Operation(
     operationId = "showTestById",
     summary = "Info for a specific test",
     tags = {"testSecond"},
     responses = {
       @ApiResponse(responseCode = "200", description = "Expected response to a valid request", content = @Content(mediaType = "application/json", schema = @Schema(implementation = TestInfoSecondDTO.class))),
       @ApiResponse(responseCode = "default", description = "unexpected error", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorSecondDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/testSecond/{testId}",
    produces = {"application/json"}
  )

  default ResponseEntity<TestInfoSecondDTO> showTestById(@Parameter(name = "testId", description = "The id of the test to retrieve", required = true, schema = @Schema(description = "")) @PathVariable("testId") Integer testId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
