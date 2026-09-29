package com.sngular.multifileplugin.ndjsonstreaming.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ImportResultDTO.ImportResultDTOBuilder.class)
public class ImportResultDTO {

  @JsonProperty(value ="imported")
  private Integer imported;

  private ImportResultDTO(ImportResultDTOBuilder builder) {
    this.imported = builder.imported;

  }

  public static ImportResultDTO.ImportResultDTOBuilder builder() {
    return new ImportResultDTO.ImportResultDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ImportResultDTOBuilder {

    private Integer imported;

    public ImportResultDTO.ImportResultDTOBuilder imported(Integer imported) {
      this.imported = imported;
      return this;
    }

    public ImportResultDTO build() {
      ImportResultDTO importResultDTO = new ImportResultDTO(this);
      return importResultDTO;
    }
  }

  @Schema(name = "imported", required = false)
  public Integer getImported() {
    return imported;
  }
  public void setImported(Integer imported) {
    this.imported = imported;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ImportResultDTO importResultDTO = (ImportResultDTO) o;
    return Objects.equals(this.imported, importResultDTO.imported);
  }

  @Override
  public int hashCode() {
    return Objects.hash(imported);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ImportResultDTO{");
    sb.append(" imported:").append(imported);
    sb.append("}");
    return sb.toString();
  }


}
