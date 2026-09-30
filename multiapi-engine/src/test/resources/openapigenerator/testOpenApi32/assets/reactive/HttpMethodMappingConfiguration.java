package com.sngular.multifileplugin.openapi32reactive;

import java.lang.reflect.Method;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.ObjectProvider;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.cors.reactive.CorsUtils;
import org.springframework.web.reactive.accept.RequestedContentTypeResolver;
import org.springframework.web.reactive.result.condition.RequestCondition;
import org.springframework.web.reactive.result.method.RequestMappingInfo;
import org.springframework.web.reactive.result.method.annotation.RequestMappingHandlerMapping;
import org.springframework.web.server.ServerWebExchange;

/**
 * Maps the operations declared with {@link HttpMethodMapping}, whose HTTP method Spring's {@code RequestMethod} has no
 * constant for, next to Spring WebFlux's own mappings. Spring's handler mapping ignores them,
 * as they carry no {@code @RequestMapping}, and this one maps nothing else, so it can be added to an application
 * whatever else it configures. It is found by component scanning of this package, or can be imported with
 * {@code @Import(HttpMethodMappingConfiguration.class)}.
 */
@Configuration(proxyBeanMethods = false)
public class HttpMethodMappingConfiguration {

  /** Before Spring's own mappings, which would otherwise answer 405 to the method they do not know. */
  private static final int ORDER = -1;

  @Bean
  public HttpMethodHandlerMapping httpMethodHandlerMapping(final ObjectProvider<RequestedContentTypeResolver> contentTypeResolver) {
    final HttpMethodHandlerMapping mapping = new HttpMethodHandlerMapping();
    mapping.setOrder(ORDER);
    contentTypeResolver.ifUnique(mapping::setContentTypeResolver);
    return mapping;
  }

  /** Maps the handler methods declared with {@link HttpMethodMapping}, and only them. */
  public static class HttpMethodHandlerMapping extends RequestMappingHandlerMapping {

    @Override
    protected RequestMappingInfo getMappingForMethod(final Method method, final Class<?> handlerType) {
      final HttpMethodMapping mapping = AnnotatedElementUtils.findMergedAnnotation(method, HttpMethodMapping.class);
      if (mapping == null) {
        return null;
      }
      RequestMappingInfo info = RequestMappingInfo.paths(resolveEmbeddedValuesInPatterns(mapping.value()))
                                                  .produces(mapping.produces())
                                                  .consumes(mapping.consumes())
                                                  .customCondition(new HttpMethodCondition(mapping.method()))
                                                  .options(builderConfiguration())
                                                  .build();
      final RequestMapping typeMapping = AnnotatedElementUtils.findMergedAnnotation(handlerType, RequestMapping.class);
      if (typeMapping != null) {
        info = RequestMappingInfo.paths(resolveEmbeddedValuesInPatterns(typeMapping.path())).options(builderConfiguration()).build().combine(info);
      }
      return info;
    }

    private RequestMappingInfo.BuilderConfiguration builderConfiguration() {
      final RequestMappingInfo.BuilderConfiguration configuration = new RequestMappingInfo.BuilderConfiguration();
      configuration.setPatternParser(getPathPatternParser());
      configuration.setContentTypeResolver(getContentTypeResolver());
      return configuration;
    }

    /**
     * Leaves a request of another method to the other mappings, and answers one of this method the way Spring does,
     * such as 415 for a body it does not consume.
     */
    @Override
    protected HandlerMethod handleNoMatch(final Set<RequestMappingInfo> infos, final ServerWebExchange exchange) throws Exception {
      final Set<RequestMappingInfo> sameMethod = infos.stream()
                                                      .filter(info -> info.getCustomCondition() instanceof HttpMethodCondition
                                                                      && ((HttpMethodCondition) info.getCustomCondition()).getMatchingCondition(exchange) != null)
                                                      .collect(Collectors.toSet());
      return sameMethod.isEmpty() ? null : super.handleNoMatch(sameMethod, exchange);
    }
  }

  /** Matches the requests of one HTTP method, and the CORS pre-flight requests asking for it. */
  static final class HttpMethodCondition implements RequestCondition<HttpMethodCondition> {

    private final String method;

    HttpMethodCondition(final String method) {
      this.method = method;
    }

    @Override
    public HttpMethodCondition combine(final HttpMethodCondition other) {
      return other;
    }

    @Override
    public HttpMethodCondition getMatchingCondition(final ServerWebExchange exchange) {
      final String requested = CorsUtils.isPreFlightRequest(exchange.getRequest())
          ? exchange.getRequest().getHeaders().getFirst("Access-Control-Request-Method")
          : exchange.getRequest().getMethodValue();
      return method.equals(requested) ? this : null;
    }

    @Override
    public int compareTo(final HttpMethodCondition other, final ServerWebExchange exchange) {
      return 0;
    }
  }
}
