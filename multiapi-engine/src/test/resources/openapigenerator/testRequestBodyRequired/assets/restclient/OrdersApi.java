package com.sngular.multifileplugin.requestbodyrequiredrestclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.requestbodyrequiredrestclient.client.ApiRestClient;

import com.sngular.multifileplugin.requestbodyrequiredrestclient.model.RetryDTO;
import com.sngular.multifileplugin.requestbodyrequiredrestclient.model.FilterDTO;
import com.sngular.multifileplugin.requestbodyrequiredrestclient.model.OrderDTO;

import com.sngular.multifileplugin.requestbodyrequiredrestclient.client.auth.Authentication;

import org.springframework.stereotype.Component;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;

@Component()
public class OrdersApi {

  private ApiRestClient apiRestClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "";

  /**
   * Builds its own ApiRestClient, sending requests to the contract's first server, and is the constructor Spring uses when the class is a component.
   */
  public OrdersApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured RestTemplate, to the contract's first server.
   */
  public OrdersApi(final ApiRestClient apiRestClient) {
    this.apiRestClient = Objects.requireNonNull(apiRestClient, "apiRestClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * root URI (RestTemplateBuilder.rootUri(...)).
   */
  public OrdersApi(final ApiRestClient apiRestClient, final String basePath) {
    this(apiRestClient);
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
    this.apiRestClient = new ApiRestClient(authenticationsApi);
  }

  /**
   * POST /orders/{orderId}/retry
   * @param orderId   (required)
   * @param retryDTO  
   * @return Retried; (status code 204)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void retryOrder(String orderId, RetryDTO retryDTO) throws RestClientException {
    retryOrderWithHttpInfo(orderId, retryDTO);
  }

  public ResponseEntity<Void> retryOrderWithHttpInfo(String orderId, RetryDTO retryDTO) throws RestClientException {

    Object postBody = retryDTO;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("orderId",  orderId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiRestClient.invokeAPI(basePath,"/orders/{orderId}/retry", HttpMethod.POST, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * POST /orders/search
   * @param filterDTO  
   * @return Searched; (status code 204)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void searchOrders(FilterDTO filterDTO) throws RestClientException {
    searchOrdersWithHttpInfo(filterDTO);
  }

  public ResponseEntity<Void> searchOrdersWithHttpInfo(FilterDTO filterDTO) throws RestClientException {

    Object postBody = filterDTO;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiRestClient.invokeAPI(basePath,"/orders/search", HttpMethod.POST, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * POST /orders
   * @param orderDTO  (required)
   * @return Created; (status code 204)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void createOrder(OrderDTO orderDTO) throws RestClientException {
    createOrderWithHttpInfo(orderDTO);
  }

  public ResponseEntity<Void> createOrderWithHttpInfo(OrderDTO orderDTO) throws RestClientException {

    Object postBody = orderDTO;
    if (orderDTO == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''orderDTO'' when calling createOrder");
    }
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiRestClient.invokeAPI(basePath,"/orders", HttpMethod.POST, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * PUT /orders/{orderId}
   * @param orderId   (required)
   * @param orderDTO  (required)
   * @return Replaced; (status code 204)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void replaceOrder(String orderId, OrderDTO orderDTO) throws RestClientException {
    replaceOrderWithHttpInfo(orderId, orderDTO);
  }

  public ResponseEntity<Void> replaceOrderWithHttpInfo(String orderId, OrderDTO orderDTO) throws RestClientException {

    Object postBody = orderDTO;
    if (orderDTO == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''orderDTO'' when calling replaceOrder");
    }
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("orderId",  orderId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiRestClient.invokeAPI(basePath,"/orders/{orderId}", HttpMethod.PUT, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

}