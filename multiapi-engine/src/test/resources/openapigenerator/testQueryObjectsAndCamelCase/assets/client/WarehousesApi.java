package com.sngular.multifileplugin.queryobjects;

import java.util.List;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.queryobjects.client.ApiRestClient;

import com.sngular.multifileplugin.queryobjects.model.WarehousePageDTO;
import com.sngular.multifileplugin.queryobjects.model.PageFilterDTO;

import com.sngular.multifileplugin.queryobjects.client.auth.Authentication;
import com.sngular.multifileplugin.queryobjects.client.auth.HttpBearerAuth;
import com.sngular.multifileplugin.queryobjects.client.auth.ApiKeyAuth;
import com.sngular.multifileplugin.queryobjects.client.auth.OAuth;

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

  private String basePath = "http://localhost:8080";

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
    this.authenticationsApi.put("bearerAuth", new HttpBearerAuth("bearer"));
    this.authenticationsApi.put("tenantId", new ApiKeyAuth("header", "X-Tenant-Id"));
    this.authenticationsApi.put("oAuth2", new OAuth());
    this.apiRestClient = new ApiRestClient(authenticationsApi);
  }

  /**
   * GET /warehouses
   * @param filters   
   * @param sort_by   
   * @param warehouse_ids   
   * @return The warehouses; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public WarehousePageDTO searchWarehouses(PageFilterDTO filters, String sort_by, List<Long> warehouse_ids) throws RestClientException {
    return searchWarehousesWithHttpInfo(filters, sort_by, warehouse_ids).getBody();
  }

  public ResponseEntity<WarehousePageDTO> searchWarehousesWithHttpInfo(PageFilterDTO filters, String sort_by, List<Long> warehouse_ids) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiRestClient.objectToQueryParams("form", true, "filters", filters));
    queryParams.putAll(apiRestClient.parameterToMultiValueMap( null, "sort_by", sort_by));
    queryParams.putAll(apiRestClient.parameterToMultiValueMap( ApiRestClient.CollectionFormat.MULTI, "warehouse_ids", warehouse_ids));
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {"tenantId", "bearerAuth", "oAuth2"};

    ParameterizedTypeReference<WarehousePageDTO> localVarReturnType = new ParameterizedTypeReference<WarehousePageDTO>() {};
    return apiRestClient.invokeAPI(basePath,"/warehouses", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * GET /warehouses/by-flat-filter
   * @param filters   
   * @return The warehouses; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public WarehousePageDTO searchWarehousesFlat(PageFilterDTO filters) throws RestClientException {
    return searchWarehousesFlatWithHttpInfo(filters).getBody();
  }

  public ResponseEntity<WarehousePageDTO> searchWarehousesFlatWithHttpInfo(PageFilterDTO filters) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiRestClient.objectToQueryParams("form", false, "filters", filters));
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {"tenantId", "bearerAuth", "oAuth2"};

    ParameterizedTypeReference<WarehousePageDTO> localVarReturnType = new ParameterizedTypeReference<WarehousePageDTO>() {};
    return apiRestClient.invokeAPI(basePath,"/warehouses/by-flat-filter", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * GET /warehouses/by-deep-filter
   * @param filters   
   * @return The warehouses; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public WarehousePageDTO searchWarehousesDeep(PageFilterDTO filters) throws RestClientException {
    return searchWarehousesDeepWithHttpInfo(filters).getBody();
  }

  public ResponseEntity<WarehousePageDTO> searchWarehousesDeepWithHttpInfo(PageFilterDTO filters) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiRestClient.objectToQueryParams("deepObject", true, "filters", filters));
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {"tenantId", "bearerAuth", "oAuth2"};

    ParameterizedTypeReference<WarehousePageDTO> localVarReturnType = new ParameterizedTypeReference<WarehousePageDTO>() {};
    return apiRestClient.invokeAPI(basePath,"/warehouses/by-deep-filter", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

}