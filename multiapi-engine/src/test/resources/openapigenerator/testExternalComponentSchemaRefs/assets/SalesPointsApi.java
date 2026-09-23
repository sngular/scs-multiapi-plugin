package com.sngular.multifileplugin.externalcomponentschemarefs;

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

import com.sngular.multifileplugin.externalcomponentschemarefs.model.SalesPointDetailDTO;
import com.sngular.multifileplugin.externalcomponentschemarefs.model.OpeningSlotDTO;

public interface SalesPointsApi {

  /**
   * GET /sales-points/{id}
   * @param id true
   * @return  The sales point; (status code 200)
   */

  @Operation(
    operationId = "getSalesPoint",
    tags = {"SalesPoints"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The sales point", content = @Content(mediaType = "application/json", schema = @Schema(implementation = SalesPointDetailDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/sales-points/{id}",
    produces = {"application/json"}
  )

  default ResponseEntity<SalesPointDetailDTO> getSalesPoint(@Parameter(name = "id", required = true, schema = @Schema(description = "")) @PathVariable("id") Long id) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * PUT /sales-points/{id}
   * @param id true
   * @param listOpeningSlotDTO (required)
   * @return  Saved; (status code 200)
   */

  @Operation(
    operationId = "saveOpeningSlots",
    tags = {"SalesPoints"},
    responses = {
      @ApiResponse(responseCode = "200", description = "Saved")
    }
  )
  @RequestMapping(
    method = RequestMethod.PUT,
    value = "/sales-points/{id}",
    produces = {"application/json"}
  )

  default ResponseEntity<Void> saveOpeningSlots(@Parameter(name = "id", required = true, schema = @Schema(description = "")) @PathVariable("id") Long id , @Parameter(name = "listOpeningSlotDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody List<OpeningSlotDTO> listOpeningSlotDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
