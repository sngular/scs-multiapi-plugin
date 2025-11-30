package com.sngular.api.generator.plugin.common.template;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sngular.api.generator.plugin.openapi.template.ClasspathTemplateLoader;
import freemarker.cache.TemplateLoader;

public abstract class CommonTemplateLoader implements TemplateLoader {

  protected static final ClassLoader LOADER = ClasspathTemplateLoader.class.getClassLoader();

  protected static final List<String> TEMPLATE_MODEL_FILES = List.of(CommonTemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA,
                                                                     CommonTemplateIndexConstants.TEMPLATE_CONTENT_ENUM,
                                                                     CommonTemplateIndexConstants.TEMPLATE_CONTENT_SCHEMA_LOMBOK,
                                                                     CommonTemplateIndexConstants.TEMPLATE_MODEL_EXCEPTION);

  protected static final List<String> TEMPLATE_ANNOTATION_FILES = List.of(CommonTemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_INTEGER_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_INTEGER_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_DOUBLE_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_DOUBLE_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_FLOAT_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_FLOAT_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_INTEGER_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_INTEGER_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_DOUBLE_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_DOUBLE_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_FLOAT_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_FLOAT_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_SIZE_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_SIZE_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_PATTERN_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_PATTERN_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MULTIPLEOF_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MULTIPLEOF_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_ITEMS_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MAX_ITEMS_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_ITEMS_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_MIN_ITEMS_VALIDATOR_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_ANNOTATION,
                                                                          CommonTemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_VALIDATOR_ANNOTATION);

  private final Map<String, String> templatesMap = new HashMap<>();

  protected CommonTemplateLoader() {

  }

  protected String readFile(final InputStream file) throws IOException {
    return new String(file.readAllBytes());
  }

  protected void init(final Map<String, String> resourceFiles) {
    templatesMap.putAll(resourceFiles);
  }

  @Override
  public final Object findTemplateSource(final String templateName) {
    return templatesMap.get(templateName);
  }

  @Override
  public final long getLastModified(final Object o) {
    return 0;
  }

  @Override
  public final Reader getReader(final Object template, final String charSet) {
    return new StringReader(template.toString());
  }

  @Override
  public void closeTemplateSource(final Object o) {
    // Not required to implement
  }

}
