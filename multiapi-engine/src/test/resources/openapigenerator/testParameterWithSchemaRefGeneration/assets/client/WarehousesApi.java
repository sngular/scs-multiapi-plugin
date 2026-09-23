package com.sngular.multifileplugin.testparameterschemarefclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.testparameterschemarefclient.client.ApiRestClient;

import com.sngular.multifileplugin.testparameterschemarefclient.model.Warehouse;
import com.sngular.multifileplugin.testparameterschemarefclient.model.PageFilters;

import com.sngular.multifileplugin.testparameterschemarefclient.client.auth.Authentication;

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
public class WarehousesApi {

  private ApiRestClient apiRestClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "";

  /**
   * Builds its own ApiRestClient, sending requests to the contract's first server, and is the constructor Spring uses when the class is a component.
   */
  public WarehousesApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured RestTemplate, to the contract's first server.
   */
  public WarehousesApi(final ApiRestClient apiRestClient) {
    this.apiRestClient = Objects.requireNonNull(apiRestClient, "apiRestClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * root URI (RestTemplateBuilder.rootUri(...)).
   */
  public WarehousesApi(final ApiRestClient apiRestClient, final String basePath) {
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
   * GET /warehouses
   * @param page pagination and sorting 
   * @param filter a free form filter 
   * @return ok; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public Warehouse searchWarehouses(PageFilters page, Object filter) throws RestClientException {
    return searchWarehousesWithHttpInfo(page, filter).getBody();
  }

  public ResponseEntity<Warehouse> searchWarehousesWithHttpInfo(PageFilters page, Object filter) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiRestClient.parameterToMultiValueMap( null, "page", page));
    queryParams.putAll(apiRestClient.parameterToMultiValueMap( null, "filter", filter));
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Warehouse> localVarReturnType = new ParameterizedTypeReference<Warehouse>() {};
    return apiRestClient.invokeAPI(basePath,"/warehouses", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

}