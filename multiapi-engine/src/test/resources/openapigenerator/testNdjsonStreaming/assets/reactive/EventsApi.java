package com.sngular.multifileplugin.ndjsonstreamingreactive;

import java.util.List;
import java.util.Map;
import java.nio.charset.StandardCharsets;
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
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import springfox.documentation.annotations.ApiIgnore;

import com.sngular.multifileplugin.ndjsonstreamingreactive.model.InlineResponse200StreamEventsDTO;

public interface EventsApi {

  /**
   * GET /events
   * @return  One event per line; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "streamEvents",
     tags = {"events"},
     responses = {
       @ApiResponse(responseCode = "200", description = "One event per line", content = @Content(mediaType = "application/x-ndjson", schema = @Schema(implementation = InlineResponse200StreamEventsDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/events",
    produces = {"application/x-ndjson"}
  )
  default ResponseEntity<Flux<InlineResponse200StreamEventsDTO>> streamEvents(@ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}