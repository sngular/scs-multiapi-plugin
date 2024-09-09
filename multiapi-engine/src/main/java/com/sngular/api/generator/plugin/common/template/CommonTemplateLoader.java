package com.sngular.api.generator.plugin.common.template;

import com.sngular.api.generator.plugin.asyncapi.template.TemplateIndexConstants;
import freemarker.cache.TemplateLoader;

import java.util.List;

public abstract class CommonTemplateLoader implements TemplateLoader {

  protected static final List<String> TEMPLATE_MODEL_FILES = List.of( "templateSchema.ftlh",
      "templateSchemaWithLombok.ftlh", "templateModelClassException.ftlh");

  protected static final List<String> TEMPLATE_ANNOTATION_FILES = List.of(TemplateIndexConstants.TEMPLATE_NOT_NULL_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_NOT_NULL_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MAX_INTEGER_ANNOTATION, TemplateIndexConstants.TEMPLATE_MAX_INTEGER_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_ANNOTATION, TemplateIndexConstants.TEMPLATE_MAX_BIG_DECIMAL_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MAX_DOUBLE_ANNOTATION, TemplateIndexConstants.TEMPLATE_MAX_DOUBLE_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MAX_FLOAT_ANNOTATION, TemplateIndexConstants.TEMPLATE_MAX_FLOAT_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MIN_INTEGER_ANNOTATION, TemplateIndexConstants.TEMPLATE_MIN_INTEGER_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MIN_DOUBLE_ANNOTATION, TemplateIndexConstants.TEMPLATE_MIN_DOUBLE_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MIN_FLOAT_ANNOTATION, TemplateIndexConstants.TEMPLATE_MIN_FLOAT_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_ANNOTATION, TemplateIndexConstants.TEMPLATE_MIN_BIG_DECIMAL_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_SIZE_ANNOTATION, TemplateIndexConstants.TEMPLATE_SIZE_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_PATTERN_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_PATTERN_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MULTIPLEOF_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MULTIPLEOF_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MAX_ITEMS_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MAX_ITEMS_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MIN_ITEMS_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_MIN_ITEMS_VALIDATOR_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_ANNOTATION,
      TemplateIndexConstants.TEMPLATE_UNIQUE_ITEMS_VALIDATOR_ANNOTATION);
}
