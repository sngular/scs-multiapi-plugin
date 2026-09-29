package com.sngular.multifileplugin.openapi32;

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

import com.sngular.multifileplugin.openapi32.model.FilterDTO;
import com.sngular.multifileplugin.openapi32.model.ItemDTO;

public interface ItemsApi {

  /**
   * QUERY /items: Finds the items that match a filter
   * @param filterDTO (required)
   * @return  The matching items; (status code 200)
   */

  @Operation(
    operationId = "queryItems",
    summary = "Finds the items that match a filter",
    tags = {"items"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The matching items", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class)))
    }
  )
  @HttpMethodMapping(
    method = "QUERY",
    value = "/items",
    produces = {"application/json"},
    consumes = {"application/json"}
  )

  default ResponseEntity<List<ItemDTO>> queryItems(@Parameter(name = "filterDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody FilterDTO filterDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * OPTIONS /items
   * @return  The methods allowed; (status code 204)
   */

  @Operation(
    operationId = "itemsOptions",
    tags = {"items"},
    responses = {
      @ApiResponse(responseCode = "204", description = "The methods allowed")
    }
  )
  @RequestMapping(
    method = RequestMethod.OPTIONS,
    value = "/items",
    produces = {"application/json"}
  )

  default ResponseEntity<Void> itemsOptions() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * GET /items/search
   * @param name false @param page false
   * @return  The items found; (status code 200)
   */

  @Operation(
    operationId = "searchItems",
    tags = {"items"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The items found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/items/search",
    produces = {"application/json"}
  )

  default ResponseEntity<List<ItemDTO>> searchItems(@Parameter(name = "name", required = false, schema = @Schema(description = "")) @RequestParam(name = "name", required = false) String name , @Parameter(name = "page", required = false, schema = @Schema(description = "")) @RequestParam(name = "page", required = false) Integer page) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * GET /items/{itemId}
   * @param itemId true
   * @return  The item; (status code 200)  No such item; (status code 404)
   */

  @Operation(
    operationId = "getItem",
    tags = {"items"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The item", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ItemDTO.class))),
      @ApiResponse(responseCode = "404", description = "No such item")
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/items/{itemId}",
    produces = {"application/json"}
  )

  default ResponseEntity<ItemDTO> getItem(@Parameter(name = "itemId", required = true, schema = @Schema(description = "")) @PathVariable("itemId") String itemId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * HEAD /items/{itemId}
   * @param itemId true
   * @return  The item exists; (status code 200)
   */

  @Operation(
    operationId = "checkItem",
    tags = {"items"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The item exists")
    }
  )
  @RequestMapping(
    method = RequestMethod.HEAD,
    value = "/items/{itemId}",
    produces = {"application/json"}
  )

  default ResponseEntity<Void> checkItem(@Parameter(name = "itemId", required = true, schema = @Schema(description = "")) @PathVariable("itemId") String itemId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * PURGE /items/{itemId}
   * @param itemId true
   * @return  Purged from the caches; (status code 204)
   */

  @Operation(
    operationId = "purgeItem",
    tags = {"items"},
    responses = {
      @ApiResponse(responseCode = "204", description = "Purged from the caches")
    }
  )
  @HttpMethodMapping(
    method = "PURGE",
    value = "/items/{itemId}",
    produces = {"application/json"}
  )

  default ResponseEntity<Void> purgeItem(@Parameter(name = "itemId", required = true, schema = @Schema(description = "")) @PathVariable("itemId") String itemId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
