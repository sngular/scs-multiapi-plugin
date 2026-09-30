package com.sngular.multifileplugin.openapi32restclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.openapi32restclient.client.ApiRestClient;

import com.sngular.multifileplugin.openapi32restclient.model.FilterDTO;
import com.sngular.multifileplugin.openapi32restclient.model.ItemDTO;
import com.sngular.multifileplugin.openapi32restclient.model.CriteriaDTO;

import com.sngular.multifileplugin.openapi32restclient.client.auth.Authentication;

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
public class ItemsApi {

  private ApiRestClient apiRestClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "";

  /**
   * Builds its own ApiRestClient, sending requests to the contract's first server, and is the constructor Spring uses when the class is a component.
   */
  public ItemsApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured RestTemplate, to the contract's first server.
   */
  public ItemsApi(final ApiRestClient apiRestClient) {
    this.apiRestClient = Objects.requireNonNull(apiRestClient, "apiRestClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * root URI (RestTemplateBuilder.rootUri(...)).
   */
  public ItemsApi(final ApiRestClient apiRestClient, final String basePath) {
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
   * QUERY /items: Finds the items that match a filter
   * @param filterDTO  (required)
   * @return The matching items; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public List<ItemDTO> queryItems(FilterDTO filterDTO) throws RestClientException {
    return queryItemsWithHttpInfo(filterDTO).getBody();
  }

  public ResponseEntity<List<ItemDTO>> queryItemsWithHttpInfo(FilterDTO filterDTO) throws RestClientException {

    Object postBody = filterDTO;
    if (filterDTO == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''filterDTO'' when calling queryItems");
    }
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<List<ItemDTO>> localVarReturnType = new ParameterizedTypeReference<List<ItemDTO>>() {};
    return apiRestClient.invokeAPI(basePath,"/items", HttpMethod.valueOf("QUERY"), uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * OPTIONS /items
   * @return The methods allowed; (status code 204)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void itemsOptions() throws RestClientException {
    itemsOptionsWithHttpInfo();
  }

  public ResponseEntity<Void> itemsOptionsWithHttpInfo() throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

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
    return apiRestClient.invokeAPI(basePath,"/items", HttpMethod.OPTIONS, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * GET /items/search
   * @param criteria   
   * @return The items found; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public List<ItemDTO> searchItems(CriteriaDTO criteria) throws RestClientException {
    return searchItemsWithHttpInfo(criteria).getBody();
  }

  public ResponseEntity<List<ItemDTO>> searchItemsWithHttpInfo(CriteriaDTO criteria) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiRestClient.objectToQueryParams("form", true, "criteria", criteria));
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<List<ItemDTO>> localVarReturnType = new ParameterizedTypeReference<List<ItemDTO>>() {};
    return apiRestClient.invokeAPI(basePath,"/items/search", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * GET /items/{itemId}
   * @param itemId   (required)
   * @return The item; (status code 200) No such item; (status code 404)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public ItemDTO getItem(String itemId) throws RestClientException {
    return getItemWithHttpInfo(itemId).getBody();
  }

  public ResponseEntity<ItemDTO> getItemWithHttpInfo(String itemId) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("itemId",  itemId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return apiRestClient.invokeAPI(basePath,"/items/{itemId}", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * HEAD /items/{itemId}
   * @param itemId   (required)
   * @return The item exists; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void checkItem(String itemId) throws RestClientException {
    checkItemWithHttpInfo(itemId);
  }

  public ResponseEntity<Void> checkItemWithHttpInfo(String itemId) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("itemId",  itemId);
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
    return apiRestClient.invokeAPI(basePath,"/items/{itemId}", HttpMethod.HEAD, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * PURGE /items/{itemId}
   * @param itemId   (required)
   * @return Purged from the caches; (status code 204)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void purgeItem(String itemId) throws RestClientException {
    purgeItemWithHttpInfo(itemId);
  }

  public ResponseEntity<Void> purgeItemWithHttpInfo(String itemId) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("itemId",  itemId);
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
    return apiRestClient.invokeAPI(basePath,"/items/{itemId}", HttpMethod.valueOf("PURGE"), uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

}