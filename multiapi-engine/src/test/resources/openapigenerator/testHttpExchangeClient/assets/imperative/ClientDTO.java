package com.sngular.multifileplugin.httpexchange.model;

import java.util.Objects;

import tools.jackson.databind.annotation.JsonDeserialize;
import tools.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.httpexchange.model.exception.ModelClassException;
import com.sngular.multifileplugin.httpexchange.model.customvalidator.NotNull;

@JsonDeserialize(builder = ClientDTO.ClientDTOBuilder.class)
public class ClientDTO {

  @JsonProperty(value ="name")
  @NotNull
  private final String name;
  @JsonProperty(value ="client_id")
  private Long client_id;

  private ClientDTO(ClientDTOBuilder builder) {
    this.name = builder.name;
    this.client_id = builder.client_id;

    validateRequiredAttributes();
  }

  public static ClientDTO.ClientDTOBuilder builder() {
    return new ClientDTO.ClientDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ClientDTOBuilder {

    private String name;
    private Long client_id;

    public ClientDTO.ClientDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ClientDTO.ClientDTOBuilder client_id(Long client_id) {
      this.client_id = client_id;
      return this;
    }

    public ClientDTO build() {
      ClientDTO clientDTO = new ClientDTO(this);
      return clientDTO;
    }
  }

  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  @Schema(name = "client_id", required = false)
  public Long getClient_id() {
    return client_id;
  }
  public void setClient_id(Long client_id) {
    this.client_id = client_id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ClientDTO clientDTO = (ClientDTO) o;
    return Objects.equals(this.name, clientDTO.name) && Objects.equals(this.client_id, clientDTO.client_id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, client_id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ClientDTO{");
    sb.append(" name:").append(name).append(",");
    sb.append(" client_id:").append(client_id);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ClientDTO");
    }
  }

}
