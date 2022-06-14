package ${packageClient};

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
import java.util.TimeZone;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.util.StdDateFormat;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.client.reactive.ClientHttpRequest;
import org.springframework.http.codec.json.Jackson2JsonDecoder;
import org.springframework.http.codec.json.Jackson2JsonEncoder;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestClientException;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ExchangeStrategies;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.stereotype.Component;

import ${packageAuth}.Authentication;

@Component
public class ApiWebClient {
  public enum CollectionFormat {
    CSV(","), TSV("\t"), SSV(" "), PIPES("|"), MULTI(null);

     private final String separator;
     private CollectionFormat(String separator) {
       this.separator = separator;
     }

     private String collectionToString(Collection<?> collection) {
       return StringUtils.collectionToDelimitedString(collection, separator);
     }
  }

  private HttpHeaders defaultHeaders = new HttpHeaders();
  private MultiValueMap<String, String> defaultCookies = new LinkedMultiValueMap<String, String>();
  private final WebClient webClient;
  private final DateFormat dateFormat;
  private final ObjectMapper objectMapper;
  private Map<String, Authentication> authentications;

  public ApiWebClient() {
    this.dateFormat = createDefaultDateFormat();
    this.objectMapper = createDefaultObjectMapper(this.dateFormat);
    this.webClient = buildWebClient(this.objectMapper);
    authentications = new HashMap<String, Authentication>();
    authentications = Collections.unmodifiableMap(authentications);
  }

  public ApiWebClient(Map<String, Authentication> authentications) {
    this.dateFormat = createDefaultDateFormat();
    this.objectMapper = createDefaultObjectMapper(this.dateFormat);
    this.webClient = buildWebClient(this.objectMapper);
    this.authentications = Collections.unmodifiableMap(authentications);
  }

  public static DateFormat createDefaultDateFormat() {
    DateFormat dateFormat = new DateFormat() {
      private final StdDateFormat fmt = new StdDateFormat()
        .withTimeZone(TimeZone.getTimeZone("UTC"))
        .withColonInTimeZone(true);

      @Override
      public Date parse(String source) {
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

  private static ObjectMapper createDefaultObjectMapper(DateFormat dateFormat) {
    if (null == dateFormat) {
     dateFormat = createDefaultDateFormat();
    }
    ObjectMapper mapper = new ObjectMapper();
    mapper.setDateFormat(dateFormat);
    mapper.registerModule(new JavaTimeModule());
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    return mapper;
  }

  private static WebClient buildWebClient(ObjectMapper mapper) {
    ExchangeStrategies strategies = ExchangeStrategies
      .builder()
      .codecs(clientDefaultCodecsConfigurer -> {
        clientDefaultCodecsConfigurer.defaultCodecs().jackson2JsonEncoder(new Jackson2JsonEncoder(mapper, MediaType.APPLICATION_JSON));
        clientDefaultCodecsConfigurer.defaultCodecs().jackson2JsonDecoder(new Jackson2JsonDecoder(mapper, MediaType.APPLICATION_JSON));
      }).build();
    WebClient.Builder webClientBuilder = WebClient.builder().exchangeStrategies(strategies);
    return webClientBuilder.build();
  }

  public ApiWebClient addDefaultHeader(String name, String value) {
    if (defaultHeaders.containsKey(name)) {
      defaultHeaders.remove(name);
    }
    defaultHeaders.add(name, value);
    return this;
  }

  public ApiWebClient addDefaultCookie(String name, String value) {
    if (defaultCookies.containsKey(name)) {
      defaultCookies.remove(name);
    }
    defaultCookies.add(name, value);
    return this;
  }

  public String parameterToString(Object param) {
    if (param == null) {
      return "";
    } else if (param instanceof Date) {
      return dateFormat.format((Date) param);
    } else if (param instanceof OffsetDateTime) {
      return formatOffsetDateTime((OffsetDateTime) param);
    } else if (param instanceof Collection) {
      StringBuilder b = new StringBuilder();
      for(Object o : (Collection<?>) param) {
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

  private String formatOffsetDateTime(OffsetDateTime offsetDateTime) {
    return  DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(offsetDateTime);
  }

  public MultiValueMap<String, String> parameterToMultiValueMap(CollectionFormat collectionFormat, String name, Object value) {
    final MultiValueMap<String, String> params = new LinkedMultiValueMap<String, String>();

    if (name == null || name.isEmpty() || value == null) {
      return params;
    }

    if(collectionFormat == null) {
      collectionFormat = CollectionFormat.CSV;
    }

    Collection<?> valueCollection = null;
    if (value instanceof Collection) {
      valueCollection = (Collection<?>) value;
    } else {
      params.add(name, parameterToString(value));
      return params;
    }

    if (valueCollection.isEmpty()){
      return params;
    }

    if (collectionFormat.equals(CollectionFormat.MULTI)) {
      for (Object item : valueCollection) {
        params.add(name, parameterToString(item));
      }
        return params;
    }

    List<String> values = new ArrayList<String>();
    for(Object o : valueCollection) {
      values.add(parameterToString(o));
    }
    params.add(name, collectionFormat.collectionToString(values));

    return params;
  }

  private boolean isJsonMime(MediaType mediaType) {
    return mediaType != null && (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType) || mediaType.getSubtype().matches("^.*\\+json[;]?\\s*$"));
  }

  public List<MediaType> selectHeaderAccept(String[] accepts) {
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

  public MediaType selectHeaderContentType(String[] contentTypes) {
    if (contentTypes.length == 0) {
      return null;
    }
    for (String contentType : contentTypes) {
      MediaType mediaType = MediaType.parseMediaType(contentType);
      if (isJsonMime(mediaType)) {
        return mediaType;
      }
    }
    return MediaType.parseMediaType(contentTypes[0]);
  }

  protected BodyInserter<?, ? super ClientHttpRequest> selectBody(Object obj, MultiValueMap<String, Object> formParams, MediaType contentType) {
    if(MediaType.APPLICATION_FORM_URLENCODED.equals(contentType)) {
      MultiValueMap<String, String> map = new LinkedMultiValueMap<>();

      formParams
        .toSingleValueMap()
        .entrySet()
        .forEach(es -> map.add(es.getKey(), String.valueOf(es.getValue())));

      return BodyInserters.fromFormData(map);
    } else if(MediaType.MULTIPART_FORM_DATA.equals(contentType)) {
      return BodyInserters.fromMultipartData(formParams);
    } else {
      return obj != null ? BodyInserters.fromValue(obj) : null;
    }
  }

  public <T> ResponseSpec invokeAPI(String basePath, String path, HttpMethod method, Map<String, Object> pathParams, MultiValueMap<String, String> queryParams, Object body,
    HttpHeaders headerParams, MultiValueMap<String, String> cookieParams, MultiValueMap<String, Object> formParams, List<MediaType> accept, MediaType contentType, String[] authNames, ParameterizedTypeReference<T> returnType) throws RestClientException {
      final WebClient.RequestBodySpec requestBuilder = prepareRequest(basePath,path, method, pathParams, queryParams, body, headerParams, cookieParams, formParams, accept, contentType, authNames);
      return requestBuilder.retrieve();
  }

  private WebClient.RequestBodySpec prepareRequest(String basePath,String path, HttpMethod method, Map<String, Object> pathParams, MultiValueMap<String, String> queryParams,
    Object body, HttpHeaders headerParams, MultiValueMap<String, String> cookieParams, MultiValueMap<String, Object> formParams, List<MediaType> accept, MediaType contentType, String[] authNames) {
      updateParamsForAuth(authNames, queryParams, headerParams, cookieParams);

      final UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(basePath).path(path);
      if (queryParams != null) {
        builder.queryParams(queryParams);
      }

      final WebClient.RequestBodySpec requestBuilder = webClient.method(method).uri(builder.build(false).toUriString(), pathParams);
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

      requestBuilder.body(selectBody(body, formParams, contentType));
      return requestBuilder;
  }

  protected void addHeadersToRequest(HttpHeaders headers, WebClient.RequestBodySpec requestBuilder) {
    for (Entry<String, List<String>> entry : headers.entrySet()) {
      List<String> values = entry.getValue();
      for(String value : values) {
        if (value != null) {
          requestBuilder.header(entry.getKey(), value);
        }
      }
    }
  }

  protected void addCookiesToRequest(MultiValueMap<String, String> cookies, WebClient.RequestBodySpec requestBuilder) {
    for (Entry<String, List<String>> entry : cookies.entrySet()) {
      List<String> values = entry.getValue();
      for(String value : values) {
        if (value != null) {
          requestBuilder.cookie(entry.getKey(), value);
        }
      }
    }
  }

  private void updateParamsForAuth(String[] authNames, MultiValueMap<String, String> queryParams, HttpHeaders headerParams, MultiValueMap<String, String> cookieParams) {
    for (String authName : authNames) {
      Authentication auth = authentications.get(authName);
      if (auth == null) {
        throw new RestClientException("Authentication undefined: " + authName);
      }
      auth.applyToParams(queryParams, headerParams, cookieParams);
    }
  }

  public String collectionPathParameterToString(CollectionFormat collectionFormat, Collection<?> values) {
    if (CollectionFormat.MULTI.equals(collectionFormat)) {
      return parameterToString(values);
    }

    if(collectionFormat == null) {
      collectionFormat = CollectionFormat.CSV;
    }

    return collectionFormat.collectionToString(values);
  }
}
