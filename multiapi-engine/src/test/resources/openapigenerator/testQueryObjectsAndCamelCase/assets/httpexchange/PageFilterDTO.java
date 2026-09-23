package com.sngular.multifileplugin.queryobjectshttpexchange.model;

import java.util.Objects;

import tools.jackson.databind.annotation.JsonDeserialize;
import tools.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = PageFilterDTO.PageFilterDTOBuilder.class)
public class PageFilterDTO {

  @JsonProperty(value ="page_size")
  private Integer page_size;
  @JsonProperty(value ="page_number")
  private Integer page_number;

  private PageFilterDTO(PageFilterDTOBuilder builder) {
    this.page_size = builder.page_size;
    this.page_number = builder.page_number;

  }

  public static PageFilterDTO.PageFilterDTOBuilder builder() {
    return new PageFilterDTO.PageFilterDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class PageFilterDTOBuilder {

    private Integer page_size;
    private Integer page_number;

    public PageFilterDTO.PageFilterDTOBuilder page_size(Integer page_size) {
      this.page_size = page_size;
      return this;
    }

    public PageFilterDTO.PageFilterDTOBuilder page_number(Integer page_number) {
      this.page_number = page_number;
      return this;
    }

    public PageFilterDTO build() {
      PageFilterDTO pageFilterDTO = new PageFilterDTO(this);
      return pageFilterDTO;
    }
  }

  @Schema(name = "page_size", required = false)
  public Integer getPage_size() {
    return page_size;
  }
  public void setPage_size(Integer page_size) {
    this.page_size = page_size;
  }

  @Schema(name = "page_number", required = false)
  public Integer getPage_number() {
    return page_number;
  }
  public void setPage_number(Integer page_number) {
    this.page_number = page_number;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PageFilterDTO pageFilterDTO = (PageFilterDTO) o;
    return Objects.equals(this.page_size, pageFilterDTO.page_size) && Objects.equals(this.page_number, pageFilterDTO.page_number);
  }

  @Override
  public int hashCode() {
    return Objects.hash(page_size, page_number);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("PageFilterDTO{");
    sb.append(" page_size:").append(page_size).append(",");
    sb.append(" page_number:").append(page_number);
    sb.append("}");
    return sb.toString();
  }


}
