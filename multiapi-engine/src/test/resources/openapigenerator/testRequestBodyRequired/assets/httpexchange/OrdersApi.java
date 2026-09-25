package com.sngular.multifileplugin.requestbodyrequiredhttpexchange;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PatchExchange;
import org.springframework.web.service.annotation.PostExchange;
import org.springframework.web.service.annotation.PutExchange;

import com.sngular.multifileplugin.requestbodyrequiredhttpexchange.model.RetryDTO;
import com.sngular.multifileplugin.requestbodyrequiredhttpexchange.model.FilterDTO;
import com.sngular.multifileplugin.requestbodyrequiredhttpexchange.model.OrderDTO;

/**
 * Orders API, as a Spring HTTP service interface. Back it with a configured {@code RestClient}, whose
 * base URL, authentication, timeouts and message converters apply to every request:
 * <pre>
 * HttpServiceProxyFactory.builderFor(RestClientAdapter.create(restClient)).build()
 *     .createClient(OrdersApi.class);
 * </pre>
 * or, on Spring Boot 4, register it with {@code @ImportHttpServices}.
 */
@HttpExchange
public interface OrdersApi {

  /**
   * POST /orders/{orderId}/retry
   * @param orderId  (required)
   * @param retryDTO 
   * @return Retried (status code 204);
   */
  @PostExchange(url = "/orders/{orderId}/retry", contentType = "application/json")
  ResponseEntity<Void> retryOrder(@PathVariable("orderId") String orderId, @RequestBody(required = false) RetryDTO retryDTO);

  /**
   * POST /orders/search
   * @param filterDTO 
   * @return Searched (status code 204);
   */
  @PostExchange(url = "/orders/search", contentType = "application/json")
  ResponseEntity<Void> searchOrders(@RequestBody(required = false) FilterDTO filterDTO);

  /**
   * POST /orders
   * @param orderDTO  (required)
   * @return Created (status code 204);
   */
  @PostExchange(url = "/orders", contentType = "application/json")
  ResponseEntity<Void> createOrder(@RequestBody(required = true) OrderDTO orderDTO);

  /**
   * PUT /orders/{orderId}
   * @param orderId  (required)
   * @param orderDTO  (required)
   * @return Replaced (status code 204);
   */
  @PutExchange(url = "/orders/{orderId}")
  ResponseEntity<Void> replaceOrder(@PathVariable("orderId") String orderId, @RequestBody(required = true) OrderDTO orderDTO);
}
