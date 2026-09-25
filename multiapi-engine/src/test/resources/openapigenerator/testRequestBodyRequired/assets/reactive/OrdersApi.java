package com.sngular.multifileplugin.requestbodyrequiredreactive;

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

import com.sngular.multifileplugin.requestbodyrequiredreactive.model.RetryDTO;
import com.sngular.multifileplugin.requestbodyrequiredreactive.model.FilterDTO;
import com.sngular.multifileplugin.requestbodyrequiredreactive.model.OrderDTO;

public interface OrdersApi {

  /**
   * POST /orders/{orderId}/retry
   * @param  orderId  true
   * @param retryDTO
   * @return  Retried; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "retryOrder",
     tags = {"orders"},
     responses = {
       @ApiResponse(responseCode = "204", description = "Retried")
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/orders/{orderId}/retry",
    produces = {"application/json"}
  )
  default ResponseEntity<Void> retryOrder(@Parameter(name = "orderId", required = true, schema = @Schema(description = "")) @PathVariable("orderId") String orderId , @Parameter(name = "retryDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody(required = false) Mono<RetryDTO> retryDTO, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * POST /orders/search
   * @param filterDTO
   * @return  Searched; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "searchOrders",
     tags = {"orders"},
     responses = {
       @ApiResponse(responseCode = "204", description = "Searched")
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/orders/search",
    produces = {"application/json"}
  )
  default ResponseEntity<Void> searchOrders(@Parameter(name = "filterDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody(required = false) Mono<FilterDTO> filterDTO, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * POST /orders
   * @param orderDTO (required)
   * @return  Created; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "createOrder",
     tags = {"orders"},
     responses = {
       @ApiResponse(responseCode = "204", description = "Created")
     }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/orders",
    produces = {"application/json"}
  )
  default ResponseEntity<Void> createOrder(@Parameter(name = "orderDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody Mono<OrderDTO> orderDTO, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  /**
   * PUT /orders/{orderId}
   * @param  orderId  true
   * @param orderDTO (required)
   * @return  Replaced; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  @Operation(
     operationId = "replaceOrder",
     tags = {"orders"},
     responses = {
       @ApiResponse(responseCode = "204", description = "Replaced")
     }
  )
  @RequestMapping(
    method = RequestMethod.PUT,
    value = "/orders/{orderId}",
    produces = {"application/json"}
  )
  default ResponseEntity<Void> replaceOrder(@Parameter(name = "orderId", required = true, schema = @Schema(description = "")) @PathVariable("orderId") String orderId , @Parameter(name = "orderDTO", description = "", required = true, schema = @Schema(description = "")) @Valid @RequestBody Mono<OrderDTO> orderDTO, @ApiIgnore final ServerWebExchange exchange) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}