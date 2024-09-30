package com.sngular.api.generator.plugin.common.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(of = {"schemaName", "className"})
public class SchemaObject {

    private boolean isEnum;

    private String schemaName;

    private String className;

    private List<String> importList;

    private Set<SchemaFieldObject> fieldObjectList;

    private String schemaCombinator;

    private String parentPackage;

    public static final class SchemaObjectBuilder {

        private final List<String> importList = new ArrayList<>();

        private final Set<SchemaFieldObject> fieldObjectList = new HashSet<>();

        public SchemaObjectBuilder importList(final List<String> importList) {
            this.importList.addAll(importList);
            return this;
        }

        public SchemaObjectBuilder importItem(final String importItem) {
            this.importList.add(importItem);
            return this;
        }

        public SchemaObjectBuilder fieldObjectList(final Set<SchemaFieldObject> fieldObjectList) {
            this.fieldObjectList.addAll(fieldObjectList);
            return this;
        }

        public SchemaObjectBuilder fieldObject(final SchemaFieldObject fieldObject) {
            this.fieldObjectList.add(fieldObject);
            return this;
        }
    }
}
