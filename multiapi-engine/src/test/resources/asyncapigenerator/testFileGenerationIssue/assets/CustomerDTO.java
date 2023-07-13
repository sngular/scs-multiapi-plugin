package com.sngular.scsplugin.filegenerationissue.model.event.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.filegenerationissue.model.event.customvalidator.Size;
import com.sngular.scsplugin.filegenerationissue.model.event.schemas.exception.ModelClassException;
import com.sngular.scsplugin.filegenerationissue.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = CustomerDTO.CustomerDTOBuilder.class)
public class CustomerDTO {

  @JsonProperty(value ="id")
  private String id;
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
  @JsonProperty(value ="email")
  @Size(min =3, max =250)
  @NotNull
  private final String email;
  @JsonProperty(value ="username")
  @Size(min =3, max =250)
  @NotNull
  private final String username;

  private CustomerDTO(String id, String firstName, String lastName, String password, String email, String username) {
    this.id = id;
    this.firstName = firstName;
    this.lastName = lastName;
    this.password = password;
    this.email = email;
    this.username = username;

    validateRequiredAttributes();
  }

  private CustomerDTO(CustomerDTOBuilder builder) {
    this.id = builder.id;
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;
    this.password = builder.password;
    this.email = builder.email;
    this.username = builder.username;

    validateRequiredAttributes();
  }

  public static CustomerDTO.CustomerDTOBuilder builder() {
    return new CustomerDTO.CustomerDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerDTOBuilder {

    private String id;
    private String firstName;
    private String lastName;
    private String password;
    private String email;
    private String username;

    public CustomerDTO.CustomerDTOBuilder id(String id) {
      this.id = id;
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

    public CustomerDTO.CustomerDTOBuilder email(String email) {
      this.email = email;
      return this;
    }

    public CustomerDTO.CustomerDTOBuilder username(String username) {
      this.username = username;
      return this;
    }

    public CustomerDTO build() {
      CustomerDTO customerDTO = new CustomerDTO(this);
      return customerDTO;
    }
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  /**
  * Get firstName
  * @return firstName
  */
  @Schema(name = "firstName", required = true)
  public String getFirstName() {
    return firstName;
  }

  /**
  * Get lastName
  * @return lastName
  */
  @Schema(name = "lastName", required = true)
  public String getLastName() {
    return lastName;
  }

  /**
  * Get password
  * @return password
  */
  @Schema(name = "password", required = true)
  public String getPassword() {
    return password;
  }

  /**
  * Get email
  * @return email
  */
  @Schema(name = "email", required = true)
  public String getEmail() {
    return email;
  }

  /**
  * Get username
  * @return username
  */
  @Schema(name = "username", required = true)
  public String getUsername() {
    return username;
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
    return Objects.equals(this.id, customerDTO.id) && Objects.equals(this.firstName, customerDTO.firstName) && Objects.equals(this.lastName, customerDTO.lastName) && Objects.equals(this.password, customerDTO.password) && Objects.equals(this.email, customerDTO.email) && Objects.equals(this.username, customerDTO.username);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, firstName, lastName, password, email, username);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerDTO{");
    sb.append(" id:").append(id).append(",");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName).append(",");
    sb.append(" password:").append(password).append(",");
    sb.append(" email:").append(email).append(",");
    sb.append(" username:").append(username).append(",");
    sb.append("}");
    return sb.toString();
  }



  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.firstName)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.lastName)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.password)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.email)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.username)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("CustomerDTO");
    }
  }

}
