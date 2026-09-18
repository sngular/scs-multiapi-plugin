package com.sngular.multifileplugin.testrarecharsnames;

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

import com.sngular.multifileplugin.testrarecharsnames.model.Shipment;

public interface ShipmentApi {

  /**
   * POST /shipment/{shipment-id}
   * @param idempotencyKey the idempotency key true @param sortBy the field to sort by false @param sessionId the session cookie false @param shipmentId the shipment to update true
   * @param shipment (required)
   * @return  ok; (status code 200)
   */

  @Operation(
    operationId = "createShipment",
    tags = {"test"},
    responses = {
      @ApiResponse(responseCode = "200", description = "ok", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Shipment.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/shipment/{shipment-id}",
    produces = {"application/json"}
  )

  default ResponseEntity<Shipment> createShipment(@Parameter(name = "Idempotency-Key", description = "the idempotency key", required = true, schema = @Schema(description = "")) @RequestHeader(name = "Idempotency-Key", required = true) String idempotencyKey , @Parameter(name = "sort-by", description = "the field to sort by", required = false, schema = @Schema(description = "")) @RequestParam(name = "sort-by", required = false) String sortBy , @Parameter(name = "session.id", description = "the session cookie", required = false, schema = @Schema(description = "")) @CookieValue(name = "session.id", required = false) String sessionId , @Parameter(name = "shipment-id", description = "the shipment to update", required = true, schema = @Schema(description = "")) @PathVariable("shipment-id") String shipmentId , @Parameter(name = "shipment", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody Shipment shipment) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
