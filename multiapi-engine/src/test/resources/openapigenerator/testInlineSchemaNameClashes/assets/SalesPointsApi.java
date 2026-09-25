package com.sngular.multifileplugin.inlineschemanameclashes;

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

import com.sngular.multifileplugin.inlineschemanameclashes.model.NewSalesPointDTO;
import com.sngular.multifileplugin.inlineschemanameclashes.model.InlineResponse200RegisterSalesPointDTO;

public interface SalesPointsApi {

  /**
   * POST /sales-points
   * @param newSalesPointDTO
   * @return  The registered sales point; (status code 200)
   */

  @Operation(
    operationId = "registerSalesPoint",
    tags = {"SalesPoints"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The registered sales point", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InlineResponse200RegisterSalesPointDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/sales-points",
    produces = {"application/json"}
  )

  default ResponseEntity<InlineResponse200RegisterSalesPointDTO> registerSalesPoint(@Parameter(name = "newSalesPointDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody(required = false) NewSalesPointDTO newSalesPointDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
