package com.sngular.multifileplugin.testreservedwords;

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

import com.sngular.multifileplugin.testreservedwords.model.Shipment;

public interface ShipmentApi {

  /**
   * GET /shipment
   * @return  ok; (status code 200)
   */

  @Operation(
    operationId = "getShipment",
    tags = {"test"},
    responses = {
      @ApiResponse(responseCode = "200", description = "ok", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Shipment.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/shipment",
    produces = {"application/json"}
  )

  default ResponseEntity<Shipment> getShipment() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
