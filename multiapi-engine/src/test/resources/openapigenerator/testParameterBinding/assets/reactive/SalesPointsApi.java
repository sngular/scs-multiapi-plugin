package com.sngular.multifileplugin.parameterbindingreactive;

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

import org.springframework.format.annotation.DateTimeFormat;
import com.sngular.multifileplugin.parameterbindingreactive.model.SalesPointPageDTO;
import com.sngular.multifileplugin.parameterbindingreactive.model.java.time.LocalDate;
import com.sngular.multifileplugin.parameterbindingreactive.model.java.time.LocalDateTime;

public interface SalesPointsApi {

  /**
   * GET /sales-points
   * @param  page_num  false start_date  true updated_after  false xChannel  false
   * @return  The page of sales points; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
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
  default ResponseEntity<Mono<SalesPointPageDTO>> searchSalesPoints(@Parameter(name = "page_num", required = false, schema = @Schema(description = "")) @RequestParam(name = "page_num", required = false, defaultValue = "0") Integer page_num , @Parameter(name = "start_date", required = true, schema = @Schema(description = "")) @RequestParam(name = "start_date", required = true) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate start_date , @Parameter(name = "updated_after", required = false, schema = @Schema(description = "")) @RequestParam(name = "updated_after", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime updated_after , @Parameter(name = "X-Channel", required = false, schema = @Schema(description = "")) @RequestHeader(name = "X-Channel", required = false, defaultValue = "web") String xChannel, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}