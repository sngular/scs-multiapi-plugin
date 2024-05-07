package com.sngular.multifileplugin.restclientWithRequestObjects.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.ParsePosition;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.TimeZone;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.util.StdDateFormat;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.RequestEntity.BodyBuilder;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.BufferingClientHttpRequestFactory;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.AbstractJackson2HttpMessageConverter;

import com.sngular.multifileplugin.restclientWithRequestObjects.client.auth.Authentication;

@Component
public class ApiRestClient {
  public enum CollectionFormat {
    CSV(","), TSV("\t"), SSV(" "), PIPES("|"), MULTI(null);

    private final String separator;
    private CollectionFormat(final String separator) {
      this.separator = separator;
    }

    private String collectionToString(final Collection<?> collection) {
      return StringUtils.collectionToDelimitedString(collection, separator);
    }
  }

  private final HttpHeaders defaultHeaders = new HttpHeaders();
  private final MultiValueMap<String, String> defaultCookies = new LinkedMultiValueMap<String, String>();
  private final RestTemplate restTemplate;
  private final DateFormat dateFormat;
  private final Map<String, Authentication> authentications;

  public ApiRestClient() {
    this.dateFormat = createDefaultDateFormat();
    addDefaultHeader("User-Agent", "Java-SDK");
    this.restTemplate = buildRestTemplate();
    authentications = Collections.unmodifiableMap(new HashMap<String, Authentication>());
  }

  public ApiRestClient(final Map<String, Authentication> authentications) {
    this.dateFormat = createDefaultDateFormat();
    addDefaultHeader("User-Agent", "Java-SDK");
    this.restTemplate = buildRestTemplate();
    this.authentications = Collections.unmodifiableMap(authentications);
  }

  public static DateFormat createDefaultDateFormat() {
    final DateFormat dateFormat = new DateFormat() {
      private final StdDateFormat fmt = new StdDateFormat()
        .withTimeZone(TimeZone.getTimeZone("UTC"))
        .withColonInTimeZone(true);

      @Override
      public Date parse(final String source) {
        return parse(source, new ParsePosition(0));
      }

      @Override
      public StringBuffer format(final Date date, final StringBuffer toAppendTo, final FieldPosition fieldPosition) {
        return fmt.format(date, toAppendTo, fieldPosition);
      }

      @Override
      public Date parse(final String source, final ParsePosition pos) {
        return fmt.parse(source, pos);
      }
    };
    dateFormat.setCalendar(new GregorianCalendar());
    dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    return dateFormat;
  }

  private static void createDefaultObjectMapper(final DateFormat defaultDateFormat, final AbstractJackson2HttpMessageConverter converter) {
    DateFormat dateFormat = defaultDateFormat;
    if (Objects.isNull(defaultDateFormat)) {
      dateFormat = createDefaultDateFormat();
    }
    final ObjectMapper mapper = converter.getObjectMapper();
    mapper.setDateFormat(dateFormat);
    mapper.registerModule(new JavaTimeModule());
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
  }

  protected RestTemplate buildRestTemplate() {
    final RestTemplate restTemplate = new RestTemplate();
    for(HttpMessageConverter converter:restTemplate.getMessageConverters()) {
      if(converter instanceof AbstractJackson2HttpMessageConverter) {
        createDefaultObjectMapper(this.dateFormat, (AbstractJackson2HttpMessageConverter) converter);
      }
    }
    restTemplate.setRequestFactory(new BufferingClientHttpRequestFactory(restTemplate.getRequestFactory()));
    return restTemplate;
  }

  public ApiRestClient addDefaultHeader(final String name, final String value) {
    if (defaultHeaders.containsKey(name)) {
      defaultHeaders.remove(name);
    }
    defaultHeaders.add(name, value);
    return this;
  }

  public ApiRestClient addDefaultCookie(final String name, final String value) {
    if (defaultCookies.containsKey(name)) {
      defaultCookies.remove(name);
    }
    defaultCookies.add(name, value);
    return this;
  }

  public String parameterToString(final Object param) {
    if (param == null) {
      return "";
    } else if (param instanceof Date) {
      return dateFormat.format((Date) param);
    } else if (param instanceof OffsetDateTime) {
      return formatOffsetDateTime((OffsetDateTime) param);
    } else if (param instanceof Collection) {
      StringBuilder b = new StringBuilder();
      for (Object o : (Collection<?>) param) {
        if(b.length() > 0) {
          b.append(",");
        }
          b.append(String.valueOf(o));
      }
      return b.toString();
    } else {
      return String.valueOf(param);
    }
  }

  private String formatOffsetDateTime(final OffsetDateTime offsetDateTime) {
    return DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(offsetDateTime);
  }

  public String collectionPathParameterToString(final CollectionFormat collectionFormat, final Collection<?> values) {
    String result;
    if (CollectionFormat.MULTI.equals(collectionFormat)) {
      result = parameterToString(values);
    }
    if(collectionFormat == null) {
      result = CollectionFormat.CSV.collectionToString(values);
    } else {
      result = collectionFormat.collectionToString(values);
    }
    return result;
  }

  public MultiValueMap<String, String> parameterToMultiValueMap(final CollectionFormat collectionFormat, final String name, final Object value) {
    final MultiValueMap<String, String> params = new LinkedMultiValueMap<String, String>();
    CollectionFormat colFormat = collectionFormat;
    if (name == null || name.isEmpty() || value == null) {
      return params;
    }

    if (colFormat == null) {
      colFormat = CollectionFormat.CSV;
    }

    if (value instanceof Map) {
      @SuppressWarnings("unchecked")
      final Map<String, Object> valuesMap = (Map<String, Object>) value;
      for (final Entry<String, Object> entry : valuesMap.entrySet()) {
        params.add(entry.getKey(), parameterToString(entry.getValue()));
      }
      return params;
    }

    Collection<?> valueCollection = null;
    if (value instanceof Collection) {
      valueCollection = (Collection<?>) value;
    } else {
      params.add(name, parameterToString(value));
      return params;
    }

    if (valueCollection.isEmpty()) {
      return params;
    }

    if (colFormat.equals(CollectionFormat.MULTI)) {
      for (Object item : valueCollection) {
        params.add(name, parameterToString(item));
      }
      return params;
    }

    List<String> values = new ArrayList<String>();
    for (Object o : valueCollection) {
      values.add(parameterToString(o));
    }
    params.add(name, colFormat.collectionToString(values));

    return params;
  }

  private boolean isJsonMime(final MediaType mediaType) {
    return mediaType != null && (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType) || mediaType.getSubtype().matches("^.*\\+json[;]?\\s*$"));
  }

  public List<MediaType> selectHeaderAccept(final String[] accepts) {
    if (accepts.length == 0) {
      return null;
    }
    for (String accept : accepts) {
      MediaType mediaType = MediaType.parseMediaType(accept);
      if (isJsonMime(mediaType) && !"application/problem+json".equalsIgnoreCase(accept)) {
        return Collections.singletonList(mediaType);
      }
    }
    return MediaType.parseMediaTypes(StringUtils.arrayToCommaDelimitedString(accepts));
  }

  public MediaType selectHeaderContentType(final String[] contentTypes) {
    if (contentTypes.length == 0) {
      return MediaType.APPLICATION_JSON;
    }
    for (String contentType : contentTypes) {
      MediaType mediaType = MediaType.parseMediaType(contentType);
      if (isJsonMime(mediaType)) {
        return mediaType;
      }
    }
    return MediaType.parseMediaType(contentTypes[0]);
  }

  public String expandPath(final String pathTemplate, final Map<String, Object> variables) {
    final DefaultUriBuilderFactory uriBuilderFactory = new DefaultUriBuilderFactory();
    uriBuilderFactory.setEncodingMode(DefaultUriBuilderFactory.EncodingMode.NONE);
    final RestTemplate restTemplate = new RestTemplate();
    restTemplate.setUriTemplateHandler(uriBuilderFactory);
    return restTemplate.getUriTemplateHandler().expand(pathTemplate, variables).toString();
  }

  protected Object selectBody(final Object obj, final MultiValueMap<String, Object> formParams, final MediaType contentType) {
    boolean isForm = MediaType.MULTIPART_FORM_DATA.isCompatibleWith(contentType) || MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(contentType);
    return isForm ? formParams : obj;
  }

  public String generateQueryUri(final MultiValueMap<String, String> queryParams, final Map<String, Object> uriParams) {
    final StringBuilder queryBuilder = new StringBuilder();
    queryParams.forEach((name, values) -> {
      try {
        final String encodedName = URLEncoder.encode(name.toString(), "UTF-8");
        if (CollectionUtils.isEmpty(values)) {
          if (queryBuilder.length() != 0) {
            queryBuilder.append('&');
          }
          queryBuilder.append(encodedName);
        } else {
          int valueItemCounter = 0;
          for (Object value : values) {
            if (queryBuilder.length() != 0) {
              queryBuilder.append('&');
            }
            queryBuilder.append(encodedName);
            if (value != null) {
              String templatizedKey = encodedName + valueItemCounter++;
              final String encodedValue = URLEncoder.encode(value.toString(), "UTF-8");
              uriParams.put(templatizedKey, encodedValue);
              queryBuilder.append('=').append("{").append(templatizedKey).append("}");
            }
          }
        }
      } catch (UnsupportedEncodingException e) {

      }
    });
    return queryBuilder.toString();
  }

  public <T> ResponseEntity<T> invokeAPI(final String basePath, final String path, final HttpMethod method, final Map<String, Object> pathParams,
    final MultiValueMap<String, String> queryParams, final Object body, final HttpHeaders headerParams, final MultiValueMap<String, String> cookieParams, final MultiValueMap<String, Object> formParams,
    final List<MediaType> accept, final MediaType contentType, final String[] authNames, final ParameterizedTypeReference<T> returnType) throws RestClientException {

      updateParamsForAuth(authNames, queryParams, headerParams, cookieParams);
      Map<String, Object> uriParams = new HashMap<>();
      uriParams.putAll(pathParams);

      String finalUri = path;

      if (queryParams != null && !queryParams.isEmpty()) {
        String queryUri = generateQueryUri(queryParams, uriParams);
        finalUri += "?" + queryUri;
      }
      String expandedPath = this.expandPath(finalUri, uriParams);
      final UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(basePath).path(expandedPath);

      URI uri;
      try {
        uri = new URI(builder.build().toUriString());
      } catch(URISyntaxException ex)  {
        throw new RestClientException("Could not build URL: " + builder.toUriString(), ex);
      }

      final BodyBuilder requestBuilder = RequestEntity.method(method, uri);
      if(accept != null) {
        requestBuilder.accept(accept.toArray(new MediaType[accept.size()]));
      }
      if(contentType != null) {
        requestBuilder.contentType(contentType);
      }

      addHeadersToRequest(headerParams, requestBuilder);
      addHeadersToRequest(defaultHeaders, requestBuilder);
      addCookiesToRequest(cookieParams, requestBuilder);
      addCookiesToRequest(defaultCookies, requestBuilder);

      RequestEntity<Object> requestEntity = requestBuilder.body(selectBody(body, formParams, contentType));

      ResponseEntity<T> responseEntity = restTemplate.exchange(requestEntity, returnType);

      if (responseEntity.getStatusCode().is2xxSuccessful()) {
        return responseEntity;
      } else {
        throw new RestClientException("API returned " + responseEntity.getStatusCode() + " and it wasn't handled by the RestTemplate error handler");
      }
  }

  protected void addHeadersToRequest(final HttpHeaders headers, final BodyBuilder requestBuilder) {
    for (Entry<String, List<String>> entry : headers.entrySet()) {
      List<String> values = entry.getValue();
      for(String value : values) {
        if (value != null) {
          requestBuilder.header(entry.getKey(), value);
        }
      }
    }
  }

  protected void addCookiesToRequest(final MultiValueMap<String, String> cookies, final BodyBuilder requestBuilder) {
    if (!cookies.isEmpty()) {
      requestBuilder.header("Cookie", buildCookieHeader(cookies));
    }
  }

  private String buildCookieHeader(final MultiValueMap<String, String> cookies) {
    final StringBuilder cookieValue = new StringBuilder();
    String delimiter = "";
    for (final Map.Entry<String, List<String>> entry : cookies.entrySet()) {
      final String value = entry.getValue().get(entry.getValue().size() - 1);
      cookieValue.append(String.format("%s%s=%s", delimiter, entry.getKey(), value));
      delimiter = "; ";
    }
    return cookieValue.toString();
  }

  private void updateParamsForAuth(final String[] authNames, final MultiValueMap<String, String> queryParams, final HttpHeaders headerParams, final MultiValueMap<String, String> cookieParams) {
    for (String authName : authNames) {
      Authentication auth = authentications.get(authName);
      if (auth == null) {
        throw new RestClientException("Authentication undefined: " + authName);
      }
      auth.applyToParams(queryParams, headerParams, cookieParams);
    }
  }

  private class ApiClientHttpRequestInterceptor implements ClientHttpRequestInterceptor {
    private final Log log = LogFactory.getLog(ApiClientHttpRequestInterceptor.class);

    @Override
    public ClientHttpResponse intercept(final HttpRequest request, final byte[] body, final ClientHttpRequestExecution execution) throws IOException {
      logRequest(request, body);
      ClientHttpResponse response = execution.execute(request, body);
      logResponse(response);
      return response;
    }

    private void logRequest(final HttpRequest request, final byte[] body) throws UnsupportedEncodingException {
      log.info("URI: " + request.getURI());
      log.info("HTTP Method: " + request.getMethod());
      log.info("HTTP Headers: " + headersToString(request.getHeaders()));
      log.info("Request Body: " + new String(body, StandardCharsets.UTF_8));
    }

    private void logResponse(final ClientHttpResponse response) throws IOException {
      log.info("HTTP Status Code: " + response.getRawStatusCode());
      log.info("Status Text: " + response.getStatusText());
      log.info("HTTP Headers: " + headersToString(response.getHeaders()));
      log.info("Response Body: " + bodyToString(response.getBody()));
    }

    private String headersToString(final HttpHeaders headers) {
      final StringBuilder builder = new StringBuilder();
      for(Entry<String, List<String>> entry : headers.entrySet()) {
        builder.append(entry.getKey()).append("=[");
          for(String value : entry.getValue()) {
            builder.append(value).append(",");
          }
          builder.setLength(builder.length() - 1);
          builder.append("],");
      }
      builder.setLength(builder.length() - 1);
      return builder.toString();
    }

    private String bodyToString(final InputStream body) throws IOException {
      final StringBuilder builder = new StringBuilder();
      final BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(body, StandardCharsets.UTF_8));
      String line = bufferedReader.readLine();
      while (line != null) {
        builder.append(line).append(System.lineSeparator());
        line = bufferedReader.readLine();
      }
      bufferedReader.close();
      return builder.toString();
    }
  }

}
