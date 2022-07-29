package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiReactiveGeneration.assets;

import java.nio.charset.StandardCharsets;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import springfox.documentation.annotations.ApiIgnore;

public interface TestApi {

  private static Mono<Void> getExampleResponse(ServerWebExchange exchange, String example) {
    return exchange.getResponse().writeWith(Mono.just(new DefaultDataBufferFactory().wrap(example.getBytes(StandardCharsets.UTF_8))));
  }

  /**
  * GET /test/{testId} : Info for a specific test
  * @param  testId The id of the test to retrieve true
  * @return  Expected response to a valid request; (status code 200)
  * @throws WebClientResponseException if an error occurs while attempting to invoke the API
  */
  @Operation(
     operationId = "showTestById",
     summary = "Info for a specific test",
     tags = { "test" },
     responses = {
          @ApiResponse(responseCode = "200", description = "Expected response to a valid request" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiTestInfoDTO.class)) ), @ApiResponse(responseCode = "default", description = "unexpected error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ApiErrorDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test/{testId}",
    produces = { "application/json" }
  )
  default Mono<ResponseEntity<ApiTestInfoDTO>> showTestById(@Parameter(name = "testId", description = "The id of the test to retrieve", required = true, schema = @Schema(description = "")) @PathVariable("testId") Integer testId, @ApiIgnore final ServerWebExchange exchange) {
    Mono<Void> result = Mono.empty();
    exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
    for (MediaType mediaType : exchange.getRequest().getHeaders().getAccept()) {
      if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
        String exampleString = "{\"error\":\"501 NOT IMPLEMENTED\"}";
        result = getExampleResponse(exchange, exampleString);
        break;
      }
    }
    return result.then(Mono.empty());
  }

  /**
  * GET /test : List all available test
  * @return  A paged array of tests; (status code 200)
  * @throws WebClientResponseException if an error occurs while attempting to invoke the API
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
    value = "/test",
    produces = { "application/json" }
  )
  default Mono<ResponseEntity<ApiTestDTO>> listTest(@ApiIgnore final ServerWebExchange exchange) {
    Mono<Void> result = Mono.empty();
    exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
    for (MediaType mediaType : exchange.getRequest().getHeaders().getAccept()) {
      if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
        String exampleString = "{\"error\":\"501 NOT IMPLEMENTED\"}";
        result = getExampleResponse(exchange, exampleString);
        break;
      }
    }
    return result.then(Mono.empty());
  }

}