package com.sngular.multifileplugin.testparameterschemaref.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = PageFilters.PageFiltersBuilder.class)
public class PageFilters {

  @JsonProperty(value ="pageSize")
  private Integer pageSize;
  @JsonProperty(value ="pageNumber")
  private Integer pageNumber;

  private PageFilters(PageFiltersBuilder builder) {
    this.pageSize = builder.pageSize;
    this.pageNumber = builder.pageNumber;

  }

  public static PageFilters.PageFiltersBuilder builder() {
    return new PageFilters.PageFiltersBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class PageFiltersBuilder {

    private Integer pageSize;
    private Integer pageNumber;

    public PageFilters.PageFiltersBuilder pageSize(Integer pageSize) {
      this.pageSize = pageSize;
      return this;
    }

    public PageFilters.PageFiltersBuilder pageNumber(Integer pageNumber) {
      this.pageNumber = pageNumber;
      return this;
    }

    public PageFilters build() {
      PageFilters pageFilters = new PageFilters(this);
      return pageFilters;
    }
  }

  @Schema(name = "pageSize", required = false)
  public Integer getPageSize() {
    return pageSize;
  }
  public void setPageSize(Integer pageSize) {
    this.pageSize = pageSize;
  }

  @Schema(name = "pageNumber", required = false)
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
    PageFilters pageFilters = (PageFilters) o;
    return Objects.equals(this.pageSize, pageFilters.pageSize) && Objects.equals(this.pageNumber, pageFilters.pageNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(pageSize, pageNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("PageFilters{");
    sb.append(" pageSize:").append(pageSize).append(",");
    sb.append(" pageNumber:").append(pageNumber);
    sb.append("}");
    return sb.toString();
  }


}
