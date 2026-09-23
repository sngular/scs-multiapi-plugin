package com.sngular.multifileplugin.camelcase.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = PageFilterDTO.PageFilterDTOBuilder.class)
public class PageFilterDTO {

  @JsonProperty(value ="page_size")
  private Integer pageSize;
  @JsonProperty(value ="page_number")
  private Integer pageNumber;

  private PageFilterDTO(PageFilterDTOBuilder builder) {
    this.pageSize = builder.pageSize;
    this.pageNumber = builder.pageNumber;

  }

  public static PageFilterDTO.PageFilterDTOBuilder builder() {
    return new PageFilterDTO.PageFilterDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class PageFilterDTOBuilder {

    private Integer pageSize;
    private Integer pageNumber;

    @JsonProperty(value ="page_size")
    public PageFilterDTO.PageFilterDTOBuilder pageSize(Integer pageSize) {
      this.pageSize = pageSize;
      return this;
    }

    @JsonProperty(value ="page_number")
    public PageFilterDTO.PageFilterDTOBuilder pageNumber(Integer pageNumber) {
      this.pageNumber = pageNumber;
      return this;
    }

    public PageFilterDTO build() {
      PageFilterDTO pageFilterDTO = new PageFilterDTO(this);
      return pageFilterDTO;
    }
  }

  @Schema(name = "page_size", required = false)
  public Integer getPageSize() {
    return pageSize;
  }
  public void setPageSize(Integer pageSize) {
    this.pageSize = pageSize;
  }

  @Schema(name = "page_number", required = false)
  public Integer getPageNumber() {
    return pageNumber;
  }
  public void setPageNumber(Integer pageNumber) {
    this.pageNumber = pageNumber;
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
    return Objects.equals(this.pageSize, pageFilterDTO.pageSize) && Objects.equals(this.pageNumber, pageFilterDTO.pageNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(pageSize, pageNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("PageFilterDTO{");
    sb.append(" page_size:").append(pageSize).append(",");
    sb.append(" page_number:").append(pageNumber);
    sb.append("}");
    return sb.toString();
  }


}
