package net.coru.scsplugin.business_model.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

public class CustomerDTO {

  @JsonProperty(value ="id")
  private String id;
  @JsonProperty(value ="firstName")
  private String firstName;
  @JsonProperty(value ="lastName")
  private String lastName;
  @JsonProperty(value ="password")
  private String password;
  @JsonProperty(value ="email")
  private String email;
  @JsonProperty(value ="username")
  private String username;

  private CustomerDTO(String id, String firstName, String lastName, String password, String email, String username) {
    this.id = id;
    this.firstName = firstName;
    this.lastName = lastName;
    this.password = password;
    this.email = email;
    this.username = username;

  }

  private CustomerDTO(CustomerDTOBuilder builder) {
    this.id = builder.id;
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;
    this.password = builder.password;
    this.email = builder.email;
    this.username = builder.username;

  }

  public static CustomerDTO.CustomerDTOBuilder builder() {
    return new CustomerDTO.CustomerDTOBuilder();
  }

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
  @Schema(name = "firstName", required = false)
  public String getFirstName() {
    return firstName;
  }
  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  /**
  * Get lastName
  * @return lastName
  */
  @Schema(name = "lastName", required = false)
  public String getLastName() {
    return lastName;
  }
  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  /**
  * Get password
  * @return password
  */
  @Schema(name = "password", required = false)
  public String getPassword() {
    return password;
  }
  public void setPassword(String password) {
    this.password = password;
  }

  /**
  * Get email
  * @return email
  */
  @Schema(name = "email", required = false)
  public String getEmail() {
    return email;
  }
  public void setEmail(String email) {
    this.email = email;
  }

  /**
  * Get username
  * @return username
  */
  @Schema(name = "username", required = false)
  public String getUsername() {
    return username;
  }
  public void setUsername(String username) {
    this.username = username;
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
    sb.append("class CustomerDTO {\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
    sb.append(" firstName: ").append(toIndentedString(firstName)).append("\n");
    sb.append(" lastName: ").append(toIndentedString(lastName)).append("\n");
    sb.append(" password: ").append(toIndentedString(password)).append("\n");
    sb.append(" email: ").append(toIndentedString(email)).append("\n");
    sb.append(" username: ").append(toIndentedString(username)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }



}
