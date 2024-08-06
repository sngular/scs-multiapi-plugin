package com.sngular.api.generator.plugin.common.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.Map;

@Data
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class CommonSpecFile {

    private String filePath;

    private String apiPackage;

    private String modelPackage;

    private String modelNamePrefix;

    private String modelNameSuffix;

    private String classNamePostfix;

    private boolean useLombokModelAnnotation;

    @Builder.Default
    private String dateTimeFormat = "yyyy-MM-dd'T'HH:mm:ss";

    @Builder.Default
    private String dateFormat = "yyyy-MM-dd";

    @Builder.Default
    private TypeConstants.TimeType useTimeType = TypeConstants.TimeType.LOCAL;

    public Map<String, String> getFormats() {
        return Map.of("DATE_TIME", dateTimeFormat, "DATE", dateFormat);
    }
}
