package com.sngular.multifileplugin.testexternalresponseref;

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

import com.sngular.multifileplugin.testexternalresponseref.model.WidgetDTO;

public interface WidgetsApi {

  /**
   * GET /widgets: List all widgets
   * @return  A list of widgets; (status code 200)  Unauthorized access; (status code 401)  Internal server error; (status code 500)
   */

  @Operation(
    operationId = "listWidgets",
    summary = "List all widgets",
    tags = {"widgets"},
    responses = {
      @ApiResponse(responseCode = "200", description = "A list of widgets", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))),
      @ApiResponse(responseCode = "401", description = "Unauthorized access"),
      @ApiResponse(responseCode = "500", description = "Internal server error")
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/widgets",
    produces = {"application/json"}
  )

  default ResponseEntity<List<WidgetDTO>> listWidgets() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * DELETE /widgets/{widgetId}: Delete a widget
   * @param widgetId true
   * @return  No content; (status code 204)  Resource not found; (status code 404)
   */

  @Operation(
    operationId = "deleteWidget",
    summary = "Delete a widget",
    tags = {"widgets"},
    responses = {
      @ApiResponse(responseCode = "204", description = "No content"),
      @ApiResponse(responseCode = "404", description = "Resource not found")
    }
  )
  @RequestMapping(
    method = RequestMethod.DELETE,
    value = "/widgets/{widgetId}",
    produces = {"application/json"}
  )

  default ResponseEntity<Void> deleteWidget(@Parameter(name = "widgetId", required = true, schema = @Schema(description = "")) @PathVariable("widgetId") String widgetId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}