package com.sngular.multifileplugin.openapi31types.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ProfileDTO.ProfileDTOBuilder.class)
public class ProfileDTO {

  @JsonProperty(value ="tags")
  private List<String> tags;
  @JsonProperty(value ="id")
  private String id;
  @JsonProperty(value ="loginCount")
  private Long loginCount;
  @JsonProperty(value ="age")
  private Integer age;
  @JsonProperty(value ="score")
  private Double score;
  @JsonProperty(value ="nickname")
  private String nickname;

  private ProfileDTO(ProfileDTOBuilder builder) {
    this.tags = builder.tags;
    this.id = builder.id;
    this.loginCount = builder.loginCount;
    this.age = builder.age;
    this.score = builder.score;
    this.nickname = builder.nickname;

  }

  public static ProfileDTO.ProfileDTOBuilder builder() {
    return new ProfileDTO.ProfileDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ProfileDTOBuilder {

    private List<String> tags = new ArrayList<String>();
    private String id;
    private Long loginCount;
    private Integer age;
    private Double score;
    private String nickname;

    public ProfileDTO.ProfileDTOBuilder tags(List<String> tags) {
      if (Objects.nonNull(tags) && !tags.isEmpty()) {
        this.tags.addAll(tags);
      }
      return this;
    }

    public ProfileDTO.ProfileDTOBuilder tag(String tag) {
      if (Objects.nonNull(tag)) {
        this.tags.add(tag);
      }
      return this;
    }

    public ProfileDTO.ProfileDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public ProfileDTO.ProfileDTOBuilder loginCount(Long loginCount) {
      this.loginCount = loginCount;
      return this;
    }

    public ProfileDTO.ProfileDTOBuilder age(Integer age) {
      this.age = age;
      return this;
    }

    public ProfileDTO.ProfileDTOBuilder score(Double score) {
      this.score = score;
      return this;
    }

    public ProfileDTO.ProfileDTOBuilder nickname(String nickname) {
      this.nickname = nickname;
      return this;
    }

    public ProfileDTO build() {
      ProfileDTO profileDTO = new ProfileDTO(this);
      return profileDTO;
    }
  }

  @Schema(name = "tags", required = false)
  public List<String> getTags() {
    return tags;
  }
  public void setTags(List<String> tags) {
    this.tags = tags;
  }

  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Schema(name = "loginCount", required = false)
  public Long getLoginCount() {
    return loginCount;
  }
  public void setLoginCount(Long loginCount) {
    this.loginCount = loginCount;
  }

  @Schema(name = "age", required = false)
  public Integer getAge() {
    return age;
  }
  public void setAge(Integer age) {
    this.age = age;
  }

  @Schema(name = "score", required = false)
  public Double getScore() {
    return score;
  }
  public void setScore(Double score) {
    this.score = score;
  }

  @Schema(name = "nickname", required = false)
  public String getNickname() {
    return nickname;
  }
  public void setNickname(String nickname) {
    this.nickname = nickname;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ProfileDTO profileDTO = (ProfileDTO) o;
    return Objects.equals(this.tags, profileDTO.tags) && Objects.equals(this.id, profileDTO.id) && Objects.equals(this.loginCount, profileDTO.loginCount) && Objects.equals(this.age, profileDTO.age) && Objects.equals(this.score, profileDTO.score) && Objects.equals(this.nickname, profileDTO.nickname);
  }

  @Override
  public int hashCode() {
    return Objects.hash(tags, id, loginCount, age, score, nickname);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ProfileDTO{");
    sb.append(" tags:").append(tags).append(",");
    sb.append(" id:").append(id).append(",");
    sb.append(" loginCount:").append(loginCount).append(",");
    sb.append(" age:").append(age).append(",");
    sb.append(" score:").append(score).append(",");
    sb.append(" nickname:").append(nickname);
    sb.append("}");
    return sb.toString();
  }


}
