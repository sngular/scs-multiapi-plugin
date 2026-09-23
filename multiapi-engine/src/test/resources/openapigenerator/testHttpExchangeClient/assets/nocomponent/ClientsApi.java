package com.sngular.multifileplugin.restclientnocomponent;

import java.time.LocalDate;
import org.springframework.web.multipart.MultipartFile;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.restclientnocomponent.client.ApiRestClient;

import com.sngular.multifileplugin.restclientnocomponent.model.ClientDTO;

import com.sngular.multifileplugin.restclientnocomponent.client.auth.Authentication;

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

public class ClientsApi {

  private ApiRestClient apiRestClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "http://localhost:8080";

  /**
   * Builds its own ApiRestClient, sending requests to the contract's first server, and is the constructor Spring uses when the class is a component.
   */
  public ClientsApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured RestTemplate, to the contract's first server.
   */
  public ClientsApi(final ApiRestClient apiRestClient) {
    this.apiRestClient = Objects.requireNonNull(apiRestClient, "apiRestClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * root URI (RestTemplateBuilder.rootUri(...)).
   */
  public ClientsApi(final ApiRestClient apiRestClient, final String basePath) {
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
   * GET /clients/{client_id}
   * @param client_id   (required)
   * @param since   
   * @param xCorrelationId   
   * @return The client; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public ClientDTO getClient(Long client_id, LocalDate since, String xCorrelationId) throws RestClientException {
    return getClientWithHttpInfo(client_id, since, xCorrelationId).getBody();
  }

  public ResponseEntity<ClientDTO> getClientWithHttpInfo(Long client_id, LocalDate since, String xCorrelationId) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("client_id",  client_id);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiRestClient.parameterToMultiValueMap( null, "since", since));
    if (xCorrelationId != null) {
      headerParams.add("X-Correlation-Id", apiRestClient.parameterToString(xCorrelationId));
    }
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ClientDTO> localVarReturnType = new ParameterizedTypeReference<ClientDTO>() {};
    return apiRestClient.invokeAPI(basePath,"/clients/{client_id}", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * DELETE /clients/{client_id}
   * @param client_id   (required)
   * @return Deleted; (status code 204)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void deleteClient(Long client_id) throws RestClientException {
    deleteClientWithHttpInfo(client_id);
  }

  public ResponseEntity<Void> deleteClientWithHttpInfo(Long client_id) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("client_id",  client_id);
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
    return apiRestClient.invokeAPI(basePath,"/clients/{client_id}", HttpMethod.DELETE, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * GET /clients
   * @param page_num   
   * @return The clients; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public List<ClientDTO> searchClients(Integer page_num) throws RestClientException {
    return searchClientsWithHttpInfo(page_num).getBody();
  }

  public ResponseEntity<List<ClientDTO>> searchClientsWithHttpInfo(Integer page_num) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiRestClient.parameterToMultiValueMap( null, "page_num", page_num));
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<List<ClientDTO>> localVarReturnType = new ParameterizedTypeReference<List<ClientDTO>>() {};
    return apiRestClient.invokeAPI(basePath,"/clients", HttpMethod.GET, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * POST /clients
   * @param clientDTO  (required)
   * @return Created; (status code 201)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public ClientDTO createClient(ClientDTO clientDTO) throws RestClientException {
    return createClientWithHttpInfo(clientDTO).getBody();
  }

  public ResponseEntity<ClientDTO> createClientWithHttpInfo(ClientDTO clientDTO) throws RestClientException {

    Object postBody = clientDTO;
    if (clientDTO == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''clientDTO'' when calling createClient");
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

    ParameterizedTypeReference<ClientDTO> localVarReturnType = new ParameterizedTypeReference<ClientDTO>() {};
    return apiRestClient.invokeAPI(basePath,"/clients", HttpMethod.POST, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

  /**
   * PUT /clients/{client_id}/document
   * @param client_id   (required)
   * @param file multipart part "file" (required)
   * @param comment multipart part "comment"
   * @return Uploaded; (status code 204)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public void uploadDocument(Long client_id, MultipartFile file, String comment) throws RestClientException {
    uploadDocumentWithHttpInfo(client_id, file, comment);
  }

  public ResponseEntity<Void> uploadDocumentWithHttpInfo(Long client_id, MultipartFile file, String comment) throws RestClientException {

    Object postBody = null;
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("client_id",  client_id);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    apiRestClient.addFormPart(formParams, "file", file);
    apiRestClient.addFormPart(formParams, "comment", comment);

    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"multipart/form-data"};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiRestClient.invokeAPI(basePath,"/clients/{client_id}/document", HttpMethod.PUT, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

}