package com.corunet.multifileplugin.testRestClient.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;

public class ApiErrorDTO {

   @JsonProperty(value ="message")
   private String message;
   @JsonProperty(value ="code")
   private Integer code;



   /**
    * Get message
    * @return message
    */
    @ApiModelProperty( value = "description")
    public String getMessage() {
       return message;
    }
    public void setMessage(String message) {
         this.message = message;
    }
   /**
    * Get code
    * @return code
    */
    @ApiModelProperty( value = "description")
    public Integer getCode() {
       return code;
    }
    public void setCode(Integer code) {
         this.code = code;
    }


     @Override
     public boolean equals(Object o) {
        if (this == o) {
          return true;
        }
        if (o == null || getClass() != o.getClass()) {
          return false;
        }
        ApiErrorDTO apiErrorDTO = (ApiErrorDTO) o;
        return Objects.equals(this.message,apiErrorDTO.message) && Objects.equals(this.code,apiErrorDTO.code) ;

     }

     @Override
     public int hashCode() {
       return Objects.hash(message,code);
     }


     @Override
     public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ApiErrorDTO 	{\n");
        sb.append("  message: ").append(toIndentedString(message)).append("\n");
        sb.append("  code: ").append(toIndentedString(code)).append("\n");
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
        return o.toString().replace("\n", "\n  ");
     }

}
