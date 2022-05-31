package com.corunet.multifileplugin.testreactiveApi.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;

public class ApiTestDTO {

   @JsonProperty(value ="name")
   private String name;
   @JsonProperty(value ="id")
   private Integer id;



   /**
    * Get name
    * @return name
    */
    @ApiModelProperty( value = "description")
    public String getName() {
       return name;
    }
    public void setName(String name) {
         this.name = name;
    }
   /**
    * Get id
    * @return id
    */
    @ApiModelProperty( value = "description")
    public Integer getId() {
       return id;
    }
    public void setId(Integer id) {
         this.id = id;
    }


     @Override
     public boolean equals(Object o) {
        if (this == o) {
          return true;
        }
        if (o == null || getClass() != o.getClass()) {
          return false;
        }
        ApiTestDTO apiTestDTO = (ApiTestDTO) o;
        return Objects.equals(this.name,apiTestDTO.name) && Objects.equals(this.id,apiTestDTO.id) ;

     }

     @Override
     public int hashCode() {
       return Objects.hash(name,id);
     }


     @Override
     public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ApiTestDTO 	{\n");
        sb.append("  name: ").append(toIndentedString(name)).append("\n");
        sb.append("  id: ").append(toIndentedString(id)).append("\n");
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
