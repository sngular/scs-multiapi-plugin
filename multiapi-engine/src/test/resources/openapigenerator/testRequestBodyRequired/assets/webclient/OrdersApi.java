package com.sngular.multifileplugin.requestbodyrequiredwebclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.requestbodyrequiredwebclient.client.ApiWebClient;
import com.sngular.multifileplugin.requestbodyrequiredwebclient.model.RetryDTO;
import com.sngular.multifileplugin.requestbodyrequiredwebclient.model.FilterDTO;
import com.sngular.multifileplugin.requestbodyrequiredwebclient.model.OrderDTO;

import com.sngular.multifileplugin.requestbodyrequiredwebclient.client.auth.Authentication;

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public class OrdersApi {

  private ApiWebClient apiWebClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "";

  /**
   * Builds its own ApiWebClient, sending requests to the contract's first server.
   */
  public OrdersApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured WebClient, to the contract's first server.
   */
  public OrdersApi(final ApiWebClient apiWebClient) {
    this.apiWebClient = Objects.requireNonNull(apiWebClient, "apiWebClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * base URL (WebClient.Builder.baseUrl(...)).
   */
  public OrdersApi(final ApiWebClient apiWebClient, final String basePath) {
    this(apiWebClient);
    this.basePath = basePath;
  }

  public String getBasePath() {
    return basePath;
  }

  public void setBasePath(final String basePath) {
    this.basePath = basePath;
  }

  protected void init() {
    this.authenticationsApi = new HashMap<String, Authentication>();
    this.apiWebClient = new ApiWebClient(authenticationsApi);
  }

  /**
   * POST /orders/{orderId}/retry: ""
   * @param orderId  true
   * @param retryDTO
   * @return Retried; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec retryOrderRequestCreation(String orderId, RetryDTO retryDTO) throws WebClientResponseException {
    Object postBody = retryDTO;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    pathParams.put("orderId",  orderId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiWebClient.invokeAPI(basePath,"/orders/{orderId}/retry", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * POST /orders/{orderId}/retry
   * @param orderId  (required)
   * @param retryDTO  
   * @return Retried; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<Void> retryOrder(String orderId, RetryDTO retryDTO) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return retryOrderRequestCreation(orderId, retryDTO).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<Void>> retryOrderWithHttpInfo(String orderId, RetryDTO retryDTO) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return retryOrderRequestCreation(orderId, retryDTO).toEntity(localVarReturnType);
  }

  /**
   * POST /orders/search: ""
   * @param filterDTO
   * @return Searched; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec searchOrdersRequestCreation(FilterDTO filterDTO) throws WebClientResponseException {
    Object postBody = filterDTO;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiWebClient.invokeAPI(basePath,"/orders/search", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * POST /orders/search
   * @param filterDTO  
   * @return Searched; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<Void> searchOrders(FilterDTO filterDTO) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return searchOrdersRequestCreation(filterDTO).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<Void>> searchOrdersWithHttpInfo(FilterDTO filterDTO) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return searchOrdersRequestCreation(filterDTO).toEntity(localVarReturnType);
  }

  /**
   * POST /orders: ""
   * @param orderDTO (required)
   * @return Created; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec createOrderRequestCreation(OrderDTO orderDTO) throws WebClientResponseException {
    Object postBody = orderDTO;
    if (orderDTO == null) {
    throw new WebClientResponseException("Missing the required parameter ''orderDTO'' when calling createOrder", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
  }
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiWebClient.invokeAPI(basePath,"/orders", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * POST /orders
   * @param orderDTO   (required)
   * @return Created; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<Void> createOrder(OrderDTO orderDTO) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return createOrderRequestCreation(orderDTO).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<Void>> createOrderWithHttpInfo(OrderDTO orderDTO) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return createOrderRequestCreation(orderDTO).toEntity(localVarReturnType);
  }

  /**
   * PUT /orders/{orderId}: ""
   * @param orderId  true
   * @param orderDTO (required)
   * @return Replaced; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec replaceOrderRequestCreation(String orderId, OrderDTO orderDTO) throws WebClientResponseException {
    Object postBody = orderDTO;
    if (orderDTO == null) {
    throw new WebClientResponseException("Missing the required parameter ''orderDTO'' when calling replaceOrder", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
  }
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    pathParams.put("orderId",  orderId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiWebClient.invokeAPI(basePath,"/orders/{orderId}", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * PUT /orders/{orderId}
   * @param orderId  (required)
   * @param orderDTO   (required)
   * @return Replaced; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<Void> replaceOrder(String orderId, OrderDTO orderDTO) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return replaceOrderRequestCreation(orderId, orderDTO).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<Void>> replaceOrderWithHttpInfo(String orderId, OrderDTO orderDTO) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return replaceOrderRequestCreation(orderId, orderDTO).toEntity(localVarReturnType);
  }

}