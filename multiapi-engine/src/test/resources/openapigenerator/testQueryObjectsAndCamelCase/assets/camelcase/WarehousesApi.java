package com.sngular.multifileplugin.camelcase;

import java.util.List;
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

import com.sngular.multifileplugin.camelcase.model.WarehousePageDTO;
import com.sngular.multifileplugin.camelcase.model.PageFilterDTO;

public interface WarehousesApi {

  /**
   * GET /warehouses
   * @param pageNumber false @param pageSize false @param sortBy false @param warehouseIds false
   * @return  The warehouses; (status code 200)
   */

  @Operation(
    operationId = "searchWarehouses",
    tags = {"Warehouses"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The warehouses", content = @Content(mediaType = "application/json", schema = @Schema(implementation = WarehousePageDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/warehouses",
    produces = {"application/json"}
  )

  default ResponseEntity<WarehousePageDTO> searchWarehouses(@Parameter(name = "page_number", required = false, schema = @Schema(description = "")) @RequestParam(name = "page_number", required = false) Integer pageNumber , @Parameter(name = "page_size", required = false, schema = @Schema(description = "")) @RequestParam(name = "page_size", required = false) Integer pageSize , @Parameter(name = "sort_by", required = false, schema = @Schema(description = "")) @RequestParam(name = "sort_by", required = false) String sortBy , @Parameter(name = "warehouse_ids", required = false, schema = @Schema(description = "")) @RequestParam(name = "warehouse_ids", required = false) List<Long> warehouseIds) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * GET /warehouses/by-flat-filter
   * @param filters false
   * @return  The warehouses; (status code 200)
   */

  @Operation(
    operationId = "searchWarehousesFlat",
    tags = {"Warehouses"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The warehouses", content = @Content(mediaType = "application/json", schema = @Schema(implementation = WarehousePageDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/warehouses/by-flat-filter",
    produces = {"application/json"}
  )

  default ResponseEntity<WarehousePageDTO> searchWarehousesFlat(@Parameter(name = "filters", required = false, schema = @Schema(description = "")) @RequestParam(name = "filters", required = false) PageFilterDTO filters) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * GET /warehouses/by-deep-filter
   * @param filtersPageNumber false @param filtersPageSize false
   * @return  The warehouses; (status code 200)
   */

  @Operation(
    operationId = "searchWarehousesDeep",
    tags = {"Warehouses"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The warehouses", content = @Content(mediaType = "application/json", schema = @Schema(implementation = WarehousePageDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/warehouses/by-deep-filter",
    produces = {"application/json"}
  )

  default ResponseEntity<WarehousePageDTO> searchWarehousesDeep(@Parameter(name = "filters[page_number]", required = false, schema = @Schema(description = "")) @RequestParam(name = "filters[page_number]", required = false) Integer filtersPageNumber , @Parameter(name = "filters[page_size]", required = false, schema = @Schema(description = "")) @RequestParam(name = "filters[page_size]", required = false) Integer filtersPageSize) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
