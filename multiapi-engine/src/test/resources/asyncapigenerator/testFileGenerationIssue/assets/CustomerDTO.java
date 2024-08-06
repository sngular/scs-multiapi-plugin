package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.filegenerationissue.model.event.customvalidator.Size;
import com.sngular.scsplugin.filegenerationissue.model.event.exception.ModelClassException;
import com.sngular.scsplugin.filegenerationissue.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = CustomerDTO.CustomerDTOBuilder.class)
public class CustomerDTO {

  @JsonProperty(value ="email")
  @Size(min =3, max =250)
  @NotNull
  private final String email;
  @JsonProperty(value ="id")
  private String id;
  @JsonProperty(value ="username")
  @Size(min =3, max =250)
  @NotNull
  private final String username;
  @JsonProperty(value ="firstName")
  @Size(min =3, max =250)
  @NotNull
  private final String firstName;
  @JsonProperty(value ="lastName")
  @Size(min =3, max =250)
  @NotNull
  private final String lastName;
  @JsonProperty(value ="password")
  @Size(min =3, max =250)
  @NotNull
  private final String password;

  private CustomerDTO(CustomerDTOBuilder builder) {
    this.email = builder.email;
    this.id = builder.id;
    this.username = builder.username;
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;
    this.password = builder.password;

    validateRequiredAttributes();
  }

  public static CustomerDTO.CustomerDTOBuilder builder() {
    return new CustomerDTO.CustomerDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerDTOBuilder {

    private String email;
    private String id;
    private String username;
    private String firstName;
    private String lastName;
    private String password;

    public CustomerDTO.CustomerDTOBuilder email(String email) {
      this.email = email;
      return this;
    }

    public CustomerDTO.CustomerDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public CustomerDTO.CustomerDTOBuilder username(String username) {
      this.username = username;
      return this;
    }

    public CustomerDTO.CustomerDTOBuilder firstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public CustomerDTO.CustomerDTOBuilder lastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public CustomerDTO.CustomerDTOBuilder password(String password) {
      this.password = password;
      return this;
    }

    public CustomerDTO build() {
      CustomerDTO customerDTO = new CustomerDTO(this);
      return customerDTO;
    }
  }

  @Schema(name = "email", required = true)
  public String getEmail() {
    return email;
  }

  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Schema(name = "username", required = true)
  public String getUsername() {
    return username;
  }

  @Schema(name = "firstName", required = true)
  public String getFirstName() {
    return firstName;
  }

  @Schema(name = "lastName", required = true)
  public String getLastName() {
    return lastName;
  }

  @Schema(name = "password", required = true)
  public String getPassword() {
    return password;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CustomerDTO customerDTO = (CustomerDTO) o;
    return Objects.equals(this.email, customerDTO.email) && Objects.equals(this.id, customerDTO.id) && Objects.equals(this.username, customerDTO.username) && Objects.equals(this.firstName, customerDTO.firstName) && Objects.equals(this.lastName, customerDTO.lastName) && Objects.equals(this.password, customerDTO.password);
  }

  @Override
  public int hashCode() {
    return Objects.hash(email, id, username, firstName, lastName, password);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerDTO{");
    sb.append(" email:").append(email).append(",");
    sb.append(" id:").append(id).append(",");
    sb.append(" username:").append(username).append(",");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName).append(",");
    sb.append(" password:").append(password);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.email)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.username)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.firstName)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.lastName)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.password)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("CustomerDTO");
    }
  }

}
