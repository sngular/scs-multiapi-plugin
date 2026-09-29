package com.sngular.multifileplugin.openapi32reactive;

import java.util.List;
import java.util.Map;
import java.nio.charset.StandardCharsets;
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
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import springfox.documentation.annotations.ApiIgnore;

import com.sngular.multifileplugin.openapi32reactive.model.FilterDTO;
import com.sngular.multifileplugin.openapi32reactive.model.ItemDTO;

public interface ItemsApi {

  /**
   * QUERY /items: Finds the items that match a filter
   * @param filterDTO (required)
   * @return  The matching items; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "queryItems",
     summary = "Finds the items that match a filter",
     tags = {"items"},
     responses = {
       @ApiResponse(responseCode = "200", description = "The matching items", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ItemDTO.class)))
     }
  )
  @HttpMethodMapping(
    method = "QUERY",
    value = "/items",
    produces = {"application/json"},
    consumes = {"application/json"}
  )
  default ResponseEntity<Flux<ItemDTO>> queryItems(@Parameter(name = "filterDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody Mono<FilterDTO> filterDTO, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * OPTIONS /items
   * @return  The methods allowed; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
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
  default ResponseEntity<Void> itemsOptions(@ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * GET /items/search
   * @param  name  false page  false
   * @return  The items found; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "searchItems",
     tags = {"items"},
     responses = {
       @ApiResponse(responseCode = "200", description = "The items found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ItemDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/items/search",
    produces = {"application/json"}
  )
  default ResponseEntity<Flux<ItemDTO>> searchItems(@Parameter(name = "name", required = false, schema = @Schema(description = "")) @RequestParam(name = "name", required = false) String name , @Parameter(name = "page", required = false, schema = @Schema(description = "")) @RequestParam(name = "page", required = false) Integer page, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * GET /items/{itemId}
   * @param  itemId  true
   * @return  The item; (status code 200)  No such item; (status code 404)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
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
  default ResponseEntity<Mono<ItemDTO>> getItem(@Parameter(name = "itemId", required = true, schema = @Schema(description = "")) @PathVariable("itemId") String itemId, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * HEAD /items/{itemId}
   * @param  itemId  true
   * @return  The item exists; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
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
  default ResponseEntity<Void> checkItem(@Parameter(name = "itemId", required = true, schema = @Schema(description = "")) @PathVariable("itemId") String itemId, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * PURGE /items/{itemId}
   * @param  itemId  true
   * @return  Purged from the caches; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
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
  default ResponseEntity<Void> purgeItem(@Parameter(name = "itemId", required = true, schema = @Schema(description = "")) @PathVariable("itemId") String itemId, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}