package com.sngular.multifileplugin.ndjsonstreaming;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.context.ApplicationContext;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;
import org.springframework.web.servlet.support.RequestContextUtils;

/**
 * Sends and reads application/x-ndjson and application/jsonl, one JSON document per line, for the API interfaces of this package. Streamed
 * responses are written by Spring MVC on its asynchronous executor, each item sent to the client as soon as it is produced,
 * and the stream the operation returns is closed once it is sent, also when sending fails. Items are serialized with the
 * application's ObjectMapper.
 */
public final class NdjsonSupport {

  private NdjsonSupport() {
  }

  /**
   * The response streaming the items of the given one as the given media type, with its status and headers. A response
   * without items, such as the NOT_IMPLEMENTED of an operation that is not implemented, is sent without a body.
   */
  public static <T> ResponseEntity<StreamingResponseBody> stream(final ResponseEntity<Stream<T>> response, final String mediaType,
      final HttpServletRequest servletRequest, final HttpServletResponse servletResponse) {
    final ResponseEntity.BodyBuilder builder = ResponseEntity.status(response.getStatusCode()).headers(response.getHeaders());
    final Stream<T> items = response.getBody();
    if (items == null) {
      return builder.build();
    }
    final ObjectMapper mapper = objectMapper(servletRequest);
    return builder.contentType(MediaType.parseMediaType(mediaType)).body(output -> write(items, mapper, output, servletResponse));
  }

  /** The response sending the items of the given one as a JSON array, with its status and headers. */
  public static <T> ResponseEntity<List<T>> collect(final ResponseEntity<Stream<T>> response) {
    final ResponseEntity.BodyBuilder builder = ResponseEntity.status(response.getStatusCode()).headers(response.getHeaders());
    final Stream<T> items = response.getBody();
    if (items == null) {
      return builder.build();
    }
    try (Stream<T> closing = items) {
      return builder.body(closing.collect(Collectors.toList()));
    }
  }

  /** The items of the request body, read one line at a time as the stream is consumed. */
  public static <T> Stream<T> read(final HttpServletRequest servletRequest, final TypeReference<T> itemType) throws IOException {
    final Iterator<T> items = objectMapper(servletRequest).readerFor(itemType).readValues(servletRequest.getInputStream());
    return StreamSupport.stream(Spliterators.spliteratorUnknownSize(items, Spliterator.ORDERED), false);
  }

  /**
   * Writes each item and sends it at once. The response is flushed rather than the output stream, whose flush Spring
   * Framework 7 ignores by default.
   */
  private static <T> void write(final Stream<T> items, final ObjectMapper mapper, final OutputStream output, final HttpServletResponse servletResponse)
      throws IOException {
    try (Stream<T> closing = items) {
      final Iterator<T> iterator = closing.iterator();
      while (iterator.hasNext()) {
        output.write(mapper.writeValueAsBytes(iterator.next()));
        output.write('\n');
        servletResponse.flushBuffer();
      }
    }
  }

  /** The application's ObjectMapper, so items are sent as its other responses are, or a default one when it has none. */
  private static ObjectMapper objectMapper(final HttpServletRequest servletRequest) {
    final ApplicationContext context = RequestContextUtils.findWebApplicationContext(servletRequest);
    ObjectMapper mapper = null;
    if (context != null) {
      mapper = context.getBeanProvider(ObjectMapper.class).getIfUnique();
    }
    if (mapper == null) {
      mapper = new ObjectMapper().findAndRegisterModules();
    }
    return mapper;
  }
}
