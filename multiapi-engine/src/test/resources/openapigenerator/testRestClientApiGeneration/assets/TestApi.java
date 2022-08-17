package net.coru.multifileplugin.testrestclient;

import net.coru.apigenerator.openapi.client.ApiRestClient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.coru.multifileplugin.testrestclient.model.ApiTestInfoDTO;
import net.coru.multifileplugin.testrestclient.model.ApiErrorDTO;
import net.coru.multifileplugin.testrestclient.model.ApiTestDTO;

import net.coru.apigenerator.openapi.client.auth.Authentication;
import net.coru.apigenerator.openapi.client.auth.HttpBasicAuth;

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

  public TestApi() {
    this.init();
  }

  protected void init() {
    this.authenticationsApi = new HashMap<String, Authentication>();
    this.authenticationsApi.put("BasicAuth", new HttpBasicAuth());
    this.apiRestClient = new ApiRestClient(authenticationsApi);
  }

  /**
  * GET /test/{testId}: Info for a specific test
  * @param testId The id of the test to retrieve true
  * @return Expected response to a valid request; (status code 200)
  * @throws RestClientException if an error occurs while attempting to invoke the API
  */
  public ApiTestInfoDTO showTestById(Integer testId) throws RestClientException {
    return showTestByIdWithHttpInfo(testId).getBody();
  }

  public ResponseEntity<ApiTestInfoDTO> showTestByIdWithHttpInfo(Integer testId) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("testId",  testId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {"BasicAuth"};

    ParameterizedTypeReference<ApiTestInfoDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestInfoDTO>() {};
    return apiRestClient.invokeAPI("http://localhost:8080/v1","/test/{testId}", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
  * GET /test: List all available test
  * @return A paged array of tests; (status code 200)
  * @throws RestClientException if an error occurs while attempting to invoke the API
  */
  public ApiTestDTO listTest() throws RestClientException {
    return listTestWithHttpInfo().getBody();
  }

  public ResponseEntity<ApiTestDTO> listTestWithHttpInfo() throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {"BasicAuth"};

    ParameterizedTypeReference<ApiTestDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestDTO>() {};
    return apiRestClient.invokeAPI("http://localhost:8080/v1","/test", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

}