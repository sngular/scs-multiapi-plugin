package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiMultiGeneration.assets;

import java.util.Optional;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import net.coru.multifileplugin.testmultifile.first.model.ErrorFirstDTO;
import net.coru.multifileplugin.testmultifile.first.model.TestFirstDTO;
import net.coru.multifileplugin.testmultifile.first.model.TestInfoFirstDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.NativeWebRequest;

public interface TestFirstApi {

  default Optional<NativeWebRequest> getRequest() {
    return Optional.empty();
  }

  /**
  * GET /testFirst : List all available test
  * @return  A paged array of tests; (status code 200)
  */

  @Operation(
     operationId = "listTest",
     summary = "List all available test",
     tags = { "testFirst" },
     responses = {
          @ApiResponse(responseCode = "200", description = "A paged array of tests" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = TestFirstDTO.class)) ), @ApiResponse(responseCode = "default", description = "unexpected error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ErrorFirstDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/testFirst",
    produces = { "application/json" }
  )

  default ResponseEntity<TestFirstDTO> listTest() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
  * GET /testFirst/{testId} : Info for a specific test
  * @param testId The id of the test to retrieve true
  * @return  Expected response to a valid request; (status code 200)
  */

  @Operation(
     operationId = "showTestById",
     summary = "Info for a specific test",
     tags = { "testFirst" },
     responses = {
          @ApiResponse(responseCode = "200", description = "Expected response to a valid request" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = TestInfoFirstDTO.class)) ), @ApiResponse(responseCode = "default", description = "unexpected error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ErrorFirstDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/testFirst/{testId}",
    produces = { "application/json" }
  )

  default ResponseEntity<TestInfoFirstDTO> showTestById(@Parameter(name = "testId", description = "The id of the test to retrieve", required = true, schema = @Schema(description = "")) @PathVariable("testId") Integer testId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
