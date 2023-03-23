package com.sngular.api.generator.plugin.openapi.model;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Set;

import lombok.Builder.Default;
import lombok.Data;

@Data
public class SchemaFieldObjectProperties {

  @Default
  private String maximum = null;

  @Default
  private String minimum = null;

  @Default
  private Boolean exclusiveMaximum = null;

  @Default
  private Boolean exclusiveMinimum = null;

  @Default
  private Boolean uniqueItems = null;

  @Default
  private Integer minItems = null;

  @Default
  private Integer maxItems = null;

  @Default
  private Integer minLength = null;

  @Default
  private Integer maxLength = null;

  @Default
  private String pattern = null;

  @Default
  private String multipleOf = null;

  private Set<String> properties;

  public SchemaFieldObjectProperties(){
    properties = new HashSet<>();
  }

  public void addAnnotations(){
    Field[] fields = this.getClass().getDeclaredFields();
    for (Field field : fields){
      field.setAccessible(true);
      if (field.getName().equalsIgnoreCase("properties"))
        continue;
      try{
        if (field.get(this) != null){
          if (field.getName().equalsIgnoreCase("maxLength") || field.getName().equalsIgnoreCase("minLength"))
            properties.add("Size");
          else properties.add(field.getName().substring(0, 1).toUpperCase() + field.getName().substring(1));
        }
      } catch (IllegalAccessException e){
        e.printStackTrace();
      }
    }
  }

}
