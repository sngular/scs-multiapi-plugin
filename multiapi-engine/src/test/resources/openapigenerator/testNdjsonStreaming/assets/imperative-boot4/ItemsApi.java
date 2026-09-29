package com.sngular.multifileplugin.ndjsonstreaming;

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
import java.util.stream.Stream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import tools.jackson.core.type.TypeReference;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.sngular.multifileplugin.ndjsonstreaming.model.ItemDTO;
import com.sngular.multifileplugin.ndjsonstreaming.model.ErrorDTO;
import com.sngular.multifileplugin.ndjsonstreaming.model.ImportResultDTO;

public interface ItemsApi {

  /**
   * GET /items, sending the items {@link #listItems} returns as application/x-ndjson, one JSON document per line.
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
  default ResponseEntity<StreamingResponseBody> listItemsNdjson(@Parameter(name = "filter", required = false, schema = @Schema(description = "")) @RequestParam(name = "filter", required = false) String filter, @Parameter(hidden = true) final HttpServletRequest servletRequest, @Parameter(hidden = true) final HttpServletResponse servletResponse) {
    return NdjsonSupport.stream(listItems(filter), "application/x-ndjson", servletRequest, servletResponse);
  }

  /**
   * GET /items. Implement this method rather than the endpoint above: the items are sent as they are produced, and the stream is closed once they are sent.
   */
  default ResponseEntity<Stream<ItemDTO>> listItems(final String filter) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * GET /items/lines, sending the items {@link #listItemLines} returns as application/jsonl, one JSON document per line.
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
  default ResponseEntity<StreamingResponseBody> listItemLinesNdjson(@Parameter(hidden = true) final HttpServletRequest servletRequest, @Parameter(hidden = true) final HttpServletResponse servletResponse) {
    return NdjsonSupport.stream(listItemLines(), "application/jsonl", servletRequest, servletResponse);
  }

  /**
   * GET /items/lines. Implement this method rather than the endpoint above: the items are sent as they are produced, and the stream is closed once they are sent.
   */
  default ResponseEntity<Stream<ItemDTO>> listItemLines() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * POST /items/lines, reading the application/jsonl body, one JSON document per line, as the items {@link #importItemLines} receives.
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
  default ResponseEntity<ImportResultDTO> importItemLinesNdjson(@Parameter(hidden = true) final HttpServletRequest servletRequest) throws IOException {
    return importItemLines(NdjsonSupport.read(servletRequest, new TypeReference<ItemDTO>() {}));
  }

  /**
   * POST /items/lines. Implement this method rather than the endpoint above: the body items are read as the stream is consumed.
   */
  default ResponseEntity<ImportResultDTO> importItemLines(final Stream<ItemDTO> itemDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * GET /items/export, sending the items {@link #exportItems} returns as application/x-ndjson, one JSON document per line.
   */
  @Operation(
    operationId = "exportItems",
    tags = {"items"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The items, as a JSON array or one per line", content = {@Content(mediaType = "application/json", schema = @Schema(implementation = List.class)), @Content(mediaType = "application/x-ndjson", schema = @Schema(implementation = ItemDTO.class))})
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/items/export",
    produces = {"application/x-ndjson"}
  )
  default ResponseEntity<StreamingResponseBody> exportItemsNdjson(@Parameter(hidden = true) final HttpServletRequest servletRequest, @Parameter(hidden = true) final HttpServletResponse servletResponse) {
    return NdjsonSupport.stream(exportItems(), "application/x-ndjson", servletRequest, servletResponse);
  }

  /**
   * GET /items/export, sending the items {@link #exportItems} returns as a JSON array.
   */
  @Operation(
    operationId = "exportItems",
    tags = {"items"},
    responses = {
      @ApiResponse(responseCode = "200", description = "The items, as a JSON array or one per line", content = {@Content(mediaType = "application/json", schema = @Schema(implementation = List.class)), @Content(mediaType = "application/x-ndjson", schema = @Schema(implementation = ItemDTO.class))})
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/items/export",
    produces = {"application/json"}
  )
  default ResponseEntity<List<ItemDTO>> exportItemsJson() {
    return NdjsonSupport.collect(exportItems());
  }

  /**
   * GET /items/export. Implement this method rather than the endpoints above: the items are sent as they are produced, and the stream is closed once they are sent.
   */
  default ResponseEntity<Stream<ItemDTO>> exportItems() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * POST /items/import, reading the application/x-ndjson body, one JSON document per line, as the items {@link #importItems} receives.
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
  default ResponseEntity<ImportResultDTO> importItemsNdjson(@Parameter(hidden = true) final HttpServletRequest servletRequest) throws IOException {
    return importItems(NdjsonSupport.read(servletRequest, new TypeReference<ItemDTO>() {}));
  }

  /**
   * POST /items/import. Implement this method rather than the endpoint above: the body items are read as the stream is consumed.
   */
  default ResponseEntity<ImportResultDTO> importItems(final Stream<ItemDTO> itemDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * GET /items/{itemId}
   * @param itemId true
   * @return  The item; (status code 200)
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

  default ResponseEntity<ItemDTO> getItem(@Parameter(name = "itemId", required = true, schema = @Schema(description = "")) @PathVariable("itemId") String itemId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
