package com.sngular.multifileplugin.ndjsonstreaming;

import java.util.Optional;
import java.util.List;
import java.util.Map;
import jakarta.validation.Valid;

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
import java.util.stream.Stream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.sngular.multifileplugin.ndjsonstreaming.model.InlineResponse200StreamEventsDTO;

public interface EventsApi {

  /**
   * GET /events, sending the items {@link #streamEvents} returns as application/x-ndjson, one JSON document per line.
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
  default ResponseEntity<StreamingResponseBody> streamEventsNdjson(@Parameter(hidden = true) final HttpServletRequest servletRequest, @Parameter(hidden = true) final HttpServletResponse servletResponse) {
    return NdjsonSupport.stream(streamEvents(), "application/x-ndjson", servletRequest, servletResponse);
  }

  /**
   * GET /events. Implement this method rather than the endpoint above: the items are sent as they are produced, and the stream is closed once they are sent.
   */
  default ResponseEntity<Stream<InlineResponse200StreamEventsDTO>> streamEvents() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }


}
