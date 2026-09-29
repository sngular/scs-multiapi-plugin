package com.sngular.multifileplugin.ndjsonstreamingreactive;

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

import com.sngular.multifileplugin.ndjsonstreamingreactive.model.ItemDTO;
import com.sngular.multifileplugin.ndjsonstreamingreactive.model.ErrorDTO;
import com.sngular.multifileplugin.ndjsonstreamingreactive.model.ImportResultDTO;

public interface ItemsApi {

  /**
   * GET /items
   * @param  filter  false
   * @return  One item per line; (status code 200)  The filter is not valid; (status code 400)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "listItems",
     tags = {"items"},
     responses = {
       @ApiResponse(responseCode = "200", description = "One item per line", content = @Content(mediaType = "application/x-ndjson", schema = @Schema(implementation = ItemDTO.class))),
       @ApiResponse(responseCode = "400", description = "The filter is not valid", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/items",
    produces = {"application/x-ndjson"}
  )
  default ResponseEntity<Flux<ItemDTO>> listItems(@Parameter(name = "filter", required = false, schema = @Schema(description = "")) @RequestParam(name = "filter", required = false) String filter, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * GET /items/lines
   * @return  One item per line, as JSON Lines; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "listItemLines",
     tags = {"items"},
     responses = {
       @ApiResponse(responseCode = "200", description = "One item per line, as JSON Lines", content = @Content(mediaType = "application/jsonl", schema = @Schema(implementation = ItemDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/items/lines",
    produces = {"application/jsonl"}
  )
  default ResponseEntity<Flux<ItemDTO>> listItemLines(@ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * POST /items/lines
   * @param itemDTO (required)
   * @return  Import summary; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "importItemLines",
     tags = {"items"},
     responses = {
       @ApiResponse(responseCode = "200", description = "Import summary", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ImportResultDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/items/lines",
    produces = {"application/json"},
    consumes = {"application/jsonl"}
  )
  default ResponseEntity<Mono<ImportResultDTO>> importItemLines(@Parameter(name = "itemDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody Flux<ItemDTO> itemDTO, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * GET /items/export
   * @return  The items, as a JSON array or one per line; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "exportItems",
     tags = {"items"},
     responses = {
       @ApiResponse(responseCode = "200", description = "The items, as a JSON array or one per line", content = {@Content(mediaType = "application/json", schema = @Schema(implementation = ItemDTO.class)), @Content(mediaType = "application/x-ndjson", schema = @Schema(implementation = ItemDTO.class))})
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/items/export",
    produces = {"application/json", "application/x-ndjson"}
  )
  default ResponseEntity<Flux<ItemDTO>> exportItems(@ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * POST /items/import
   * @param itemDTO (required)
   * @return  Import summary; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "importItems",
     tags = {"items"},
     responses = {
       @ApiResponse(responseCode = "200", description = "Import summary", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ImportResultDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/items/import",
    produces = {"application/json"},
    consumes = {"application/x-ndjson"}
  )
  default ResponseEntity<Mono<ImportResultDTO>> importItems(@Parameter(name = "itemDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody Flux<ItemDTO> itemDTO, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * GET /items/{itemId}
   * @param  itemId  true
   * @return  The item; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "getItem",
     tags = {"items"},
     responses = {
       @ApiResponse(responseCode = "200", description = "The item", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ItemDTO.class)))
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

}