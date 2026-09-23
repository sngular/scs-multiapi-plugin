package com.sngular.multifileplugin.testparameterschemaref;

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

import com.sngular.multifileplugin.testparameterschemaref.model.Warehouse;

public interface WarehousesApi {

  /**
   * GET /warehouses
   * @param pageNumber false @param pageSize false @param filter a free form filter false
   * @return  ok; (status code 200)
   */

  @Operation(
    operationId = "searchWarehouses",
    tags = {"test"},
    responses = {
      @ApiResponse(responseCode = "200", description = "ok", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Warehouse.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/warehouses",
    produces = {"application/json"}
  )

  default ResponseEntity<Warehouse> searchWarehouses(@Parameter(name = "pageNumber", required = false, schema = @Schema(description = "")) @RequestParam(name = "pageNumber", required = false) Integer pageNumber , @Parameter(name = "pageSize", required = false, schema = @Schema(description = "")) @RequestParam(name = "pageSize", required = false) Integer pageSize , @Parameter(name = "filter", description = "a free form filter", required = false, schema = @Schema(description = "")) @RequestParam(name = "filter", required = false) Object filter) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
