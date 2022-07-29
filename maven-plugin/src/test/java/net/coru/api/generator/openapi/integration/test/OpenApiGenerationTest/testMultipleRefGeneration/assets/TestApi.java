package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testMultipleRefGeneration.assets;

import java.util.Optional;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.NativeWebRequest;

public interface TestApi {

  default Optional<NativeWebRequest> getRequest() {
    return Optional.empty();
  }

  /**
  * POST /test : Start a Game
  * @return  Test File for SCC MultiApi Plugin.; (status code 200)
  */

  @Operation(
     operationId = "createGame",
     summary = "Start a Game",
     tags = { "games" },
     responses = {
          @ApiResponse(responseCode = "200", description = "Test File for SCC MultiApi Plugin." , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = InlineResponse200CreateGameDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/test",
    produces = { "application/json" }
  )

  default ResponseEntity<InlineResponse200CreateGameDTO> createGame() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
