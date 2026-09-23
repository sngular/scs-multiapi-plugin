package com.sngular.multifileplugin.parameterbinding;

import org.springframework.format.annotation.DateTimeFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
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

import com.sngular.multifileplugin.parameterbinding.model.SalesPointPageDTO;

public interface SalesPointsApi {

  /**
   * GET /sales-points
   * @param page_num false @param start_date true @param updated_after false @param xChannel false
   * @return  The page of sales points; (status code 200)
   */

  @Operation(
    operationId = "searchSalesPoints",
    tags = {"SalesPoints"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The page of sales points", content = @Content(mediaType = "application/json", schema = @Schema(implementation = SalesPointPageDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/sales-points",
    produces = {"application/json"}
  )

  default ResponseEntity<SalesPointPageDTO> searchSalesPoints(@Parameter(name = "page_num", required = false, schema = @Schema(description = "")) @RequestParam(name = "page_num", required = false, defaultValue = "0") Integer page_num , @Parameter(name = "start_date", required = true, schema = @Schema(description = "")) @RequestParam(name = "start_date", required = true) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate start_date , @Parameter(name = "updated_after", required = false, schema = @Schema(description = "")) @RequestParam(name = "updated_after", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime updated_after , @Parameter(name = "X-Channel", required = false, schema = @Schema(description = "")) @RequestHeader(name = "X-Channel", required = false, defaultValue = "web") String xChannel) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
