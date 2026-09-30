package com.sngular.multifileplugin.openapi32reactive;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Maps an operation whose HTTP method Spring's {@code RequestMethod} has no constant for, such as OpenAPI 3.2's
 * {@code QUERY} or a method of its {@code additionalOperations}, as {@code @RequestMapping} maps the others. Operations
 * declared with it are mapped by {@link HttpMethodMappingConfiguration}, and only by it.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface HttpMethodMapping {

  /** The HTTP method, as it is sent. */
  String method();

  /** The paths it is mapped to. */
  String[] value();

  /** The media types it produces, as {@code @RequestMapping#produces}. */
  String[] produces() default {};

  /** The media types it consumes, as {@code @RequestMapping#consumes}. */
  String[] consumes() default {};
}
