package com.sngular.multifileplugin.restclientWithRequestObjects;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.restclientWithRequestObjects.client.ApiRestClient;

import com.sngular.multifileplugin.restclientWithRequestObjects.model.ApiTestInputDTO;
import com.sngular.multifileplugin.restclientWithRequestObjects.model.ApiTestResponseDTO;

import com.sngular.multifileplugin.restclientWithRequestObjects.client.auth.Authentication;

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
public class TestApi {

  private ApiRestClient apiRestClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "http://localhost:8080/v1";

  /**
   * Builds its own ApiRestClient, sending requests to the contract's first server, and is the constructor Spring uses when the class is a component.
   */
  public TestApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured RestTemplate, to the contract's first server.
   */
  public TestApi(final ApiRestClient apiRestClient) {
    this.apiRestClient = Objects.requireNonNull(apiRestClient, "apiRestClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * root URI (RestTemplateBuilder.rootUri(...)).
   */
  public TestApi(final ApiRestClient apiRestClient, final String basePath) {
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
   * GET /test/form_url_encoded: Test url form encoded
   * @param apiTestInputDTO  (required)
   * @return Test response; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public ApiTestResponseDTO test_form_url_encoded(ApiTestInputDTO apiTestInputDTO) throws RestClientException {
    return test_form_url_encodedWithHttpInfo(apiTestInputDTO).getBody();
  }

  public ResponseEntity<ApiTestResponseDTO> test_form_url_encodedWithHttpInfo(ApiTestInputDTO apiTestInputDTO) throws RestClientException {

    Object postBody = apiTestInputDTO;
    if (apiTestInputDTO == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''apiTestInputDTO'' when calling test_form_url_encoded");
    }
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    apiRestClient.addFormPart(formParams, "name", apiTestInputDTO.getName());
    apiRestClient.addFormPart(formParams, "id", apiTestInputDTO.getId());

    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/x-www-form-urlencoded"};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ApiTestResponseDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestResponseDTO>() {};
    return apiRestClient.invokeAPI(basePath,"/test/form_url_encoded", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * GET /test/multipart: Test multipart
   * @param apiTestInputDTO  (required)
   * @return Test response; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public ApiTestResponseDTO test_multipart(ApiTestInputDTO apiTestInputDTO) throws RestClientException {
    return test_multipartWithHttpInfo(apiTestInputDTO).getBody();
  }

  public ResponseEntity<ApiTestResponseDTO> test_multipartWithHttpInfo(ApiTestInputDTO apiTestInputDTO) throws RestClientException {

    Object postBody = apiTestInputDTO;
    if (apiTestInputDTO == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''apiTestInputDTO'' when calling test_multipart");
    }
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    apiRestClient.addFormPart(formParams, "name", apiTestInputDTO.getName());
    apiRestClient.addFormPart(formParams, "id", apiTestInputDTO.getId());

    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"multipart/form-data"};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ApiTestResponseDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestResponseDTO>() {};
    return apiRestClient.invokeAPI(basePath,"/test/multipart", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

}