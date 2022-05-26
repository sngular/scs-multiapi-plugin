package com.corunet.api.generator.plugin.openapi.parameter;

import lombok.Data;
import org.apache.maven.plugins.annotations.Parameter;

@Data
public class FileSpec {

  @Parameter(name = "inputSpec", property = "inputSpec", required = true)
  private String inputSpec;

  @Parameter(name = "apiPackage", property = "apiPackage")
  private String apiPackage;

  @Parameter(name = "modelPackage", property = "modelPackage")
  private String modelPackage;

  @Parameter(name = "modelNamePrefix", property = "modelNamePrefix")
  private String modelNamePrefix;

  @Parameter(name = "modelNameSuffix", property = "modelNameSuffix")
  private String modelNameSuffix;

  @Parameter(name = "callMode", property = "callMode")
  private Boolean callMode = false;

  @Parameter(name = "useTagsGroup", property = "useTagsGroup")
  private Boolean useTagsGroup = false;

  @Parameter(name = "useLombokModelAnnotation", property = "useLombokModelAnnotation")
  private Boolean useLombokModelAnnotation = false;

  @Parameter(name = "isReactive", property = "isReactive")
  private Boolean isReactive = false;
}
