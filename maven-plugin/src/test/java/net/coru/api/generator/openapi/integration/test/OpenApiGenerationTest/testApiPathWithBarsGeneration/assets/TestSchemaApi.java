package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiPathWithBarsGeneration.assets;

import java.util.Optional;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiPathParameterGeneration.assets.ApiErrorDTO;
import net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiPathParameterGeneration.assets.ApiTestDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.NativeWebRequest;

public interface TestSchemaApi {

  default Optional<NativeWebRequest> getRequest() {
    return Optional.empty();
  }

  /**
  * GET /test_schema : List all available test
  * @return  A paged array of tests; (status code 200)
  */

  @Operation(
     operationId = "listTest",
     summary = "List all available test",
     tags = { "test" },
     responses = {
          @ApiResponse(responseCode = "200", description = "A paged array of tests" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiTestDTO.class)) ), @ApiResponse(responseCode = "default", description = "unexpected error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test_schema",
    produces = { "application/json" }
  )

  default ResponseEntity<ApiTestDTO> listTest() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
