package com.sngular.api.generator.plugin.common.model;

import lombok.*;

import java.util.Map;

@Data
@Builder
@AllArgsConstructor
@EqualsAndHashCode(of = "baseName")
public class SchemaFieldObject {

    private String baseName;

    @Builder.Default
    private SchemaFieldObjectType dataType = new SchemaFieldObjectType(TypeConstants.OBJECT);

    @Builder.Default
    private SchemaFieldObjectProperties restrictions = new SchemaFieldObjectProperties();

    private String importClass;

    private boolean required;

    private Map<String, String> enumValues;

    private Object constValue;
}
