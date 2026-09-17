# v7.1 Implementation Notes: Dependency-Based Spec Loading

## Completed (Phases 1-3)

✅ **Phase 1: Core Classes**
- `DependencySpecLoader` - Loads specs from Maven JARs
- `DependencyResolutionContext` - Manages loaders and caching
- `CommonSpecFile` - Extended with fromGroupId/fromArtifactId/fromVersion
- Tests: 7/7 passing

**Commit**: `2aa5807`

---

## Remaining (Phases 2, 4-5)

### Phase 2: Integration (6-8 hours)

#### 2.1 Update SchemaUtil.solveRef()

Add overloaded method with URLClassLoader parameter:

```java
public static JsonNode solveRef(
    final String refValue, 
    final Map<String, JsonNode> schemaMap, 
    final URI rootFilePath,
    final URLClassLoader jarLoader) {
  
  if (jarLoader != null) {
    return solveRefFromJar(refValue, jarLoader);
  }
  
  // Fall back to existing logic
  return solveRef(refValue, schemaMap, rootFilePath);
}

private static JsonNode solveRefFromJar(final String refValue, final URLClassLoader jarLoader) {
  if (refValue.startsWith("#")) {
    // Internal JSON ref - return null, let existing logic handle it
    return null;
  }
  
  // External file ref within same JAR
  try (InputStream stream = jarLoader.getResourceAsStream(refValue)) {
    if (stream == null) {
      LOGGER.warn("Cannot resolve ref in JAR: {}", refValue);
      return null;
    }
    return YAML_MAPPER.readTree(stream);
  } catch (final IOException e) {
    LOGGER.warn("Failed to load ref from JAR: {}", refValue, e);
    return null;
  }
}
```

**Location**: `multiapi-engine/src/main/java/.../tools/SchemaUtil.java` (line ~32)

**Changes**:
- Add import for `java.net.URLClassLoader`
- Add both overloaded methods
- Keep existing method unchanged (backward compatible)

---

#### 2.2 Update OpenApiUtil.processPaths()

Detect `fromDependency` and create resolution context:

```java
public static Map<String, JsonNode> processPaths(
    final JsonNode openApi, 
    final Map<String, JsonNode> schemaMap, 
    final SpecFile specFile) {
  
  // NEW: Handle dependency-based spec loading
  URLClassLoader jarLoader = null;
  if (specFile.usesExternalDependency()) {
    try {
      jarLoader = DependencySpecLoader.loadSpecAndGetLoader(
          specFile.getFilePath(),
          specFile.getFromGroupId(),
          specFile.getFromArtifactId(),
          specFile.getFromVersion());
    } catch (final IOException e) {
      throw new RuntimeException("Failed to load spec from dependency", e);
    }
  }
  
  // ... existing code ...
  
  // Pass jarLoader to processPathContent
  processPathContent(schemaMap, operation, specFile, jarLoader);
  
  return schemaMap;
}

private static void processPathContent(
    final Map<String, JsonNode> schemaMap,
    final JsonNode operation,
    final SpecFile specFile,
    final URLClassLoader jarLoader) {  // NEW parameter
  
  // Update calls to solveRef() to include jarLoader
  // Example: SchemaUtil.solveRef(refValue, schemaMap, baseUri, jarLoader)
}
```

**Location**: `multiapi-engine/src/main/java/.../openapi/utils/OpenApiUtil.java` (line ~181)

**Changes**:
- Add import for `DependencySpecLoader`
- Check `specFile.usesExternalDependency()`
- Load JAR and get URLClassLoader
- Pass jarLoader through method chain

---

#### 2.3 Update MapperPathUtil.mapResponseObject()

Use JAR loader when resolving response types:

```java
private static List<ContentObject> mapContentObject(
    final SpecFile specFile,
    final JsonNode content,
    final String inlineObject,
    final GlobalObject globalObject,
    final Path baseDir,
    final URLClassLoader jarLoader) {  // NEW parameter
  
  // ... existing code ...
  
  // When calling getSchemaType, pass jarLoader
  final SchemaFieldObjectType dataType = getSchemaType(
      schema, pojoName, specFile, globalObject, baseDir, jarLoader);
  
  return contentObjects;
}

private static SchemaFieldObjectType getSchemaType(
    final JsonNode schema,
    final String pojoName,
    final SpecFile specFile,
    final GlobalObject globalObject,
    final Path baseDir,
    final URLClassLoader jarLoader) {  // NEW parameter
  
  if (ApiTool.hasRef(schema)) {
    final String refValue = ApiTool.getRefValue(schema);
    
    // NEW: Try JAR loader first if available
    if (jarLoader != null && !refValue.startsWith("#")) {
      try {
        final JsonNode jarRef = DependencySpecLoader.loadSpecFromJar(refValue, jarLoader);
        if (jarRef != null) {
          return getSchemaType(jarRef, pojoName, specFile, globalObject, baseDir, jarLoader);
        }
      } catch (final IOException e) {
        LOGGER.debug("Failed to load ref from JAR: {}", refValue, e);
      }
    }
    
    // Fall back to existing logic
    // ... rest of method
  }
}
```

**Location**: `multiapi-engine/src/main/java/.../openapi/utils/MapperPathUtil.java` (line ~391)

**Changes**:
- Add URLClassLoader parameter to relevant methods
- Try JAR loader first for external refs
- Fall back to existing logic if JAR loader not available

---

#### 2.4 Update OpenApiGenerator.java

Wire up the new flow:

```java
private void generateApi(final SpecFile specFile) throws IOException {
  final JsonNode openApi = ApiTool.getPojoFromSpecFile(baseDir, specFile);
  
  // NEW: Load spec from dependency if specified
  final JsonNode spec;
  if (specFile.usesExternalDependency()) {
    spec = DependencySpecLoader.loadSpec(
        specFile.getFilePath(),
        specFile.getFromGroupId(),
        specFile.getFromArtifactId(),
        specFile.getFromVersion());
  } else {
    spec = openApi;
  }
  
  // Use spec for processing
  final Map<String, JsonNode> schemaMap = OpenApiUtil.processPaths(spec, new HashMap<>(), specFile);
  
  // ... rest of method
}
```

**Location**: `multiapi-engine/src/main/java/.../openapi/OpenApiGenerator.java`

**Changes**:
- Detect `fromDependency` in specFile
- Load spec from dependency if specified
- Pass to existing processing pipeline

---

### Phase 4: Gradle Plugin Support (2-3 hours)

#### 4.1 Update AsyncApiTask.groovy

```groovy
class AsyncApiTask extends DefaultTask {
  
  @Input
  @Optional
  String fromGroupId
  
  @Input
  @Optional
  String fromArtifactId
  
  @Input
  @Optional
  String fromVersion
  
  @TaskAction
  void generateAsyncApi() {
    // Pass to SpecFile configuration
    specFile.fromGroupId = fromGroupId
    specFile.fromArtifactId = fromArtifactId
    specFile.fromVersion = fromVersion
  }
}
```

**Location**: `scs-multiapi-gradle-plugin/src/main/groovy/.../AsyncApiTask.groovy`

#### 4.2 Update OpenApiTask.groovy

Same pattern as AsyncApiTask.

**Location**: `scs-multiapi-gradle-plugin/src/main/groovy/.../OpenApiTask.groovy`

#### 4.3 Update model classes

```groovy
// OperationParameter.groovy or similar
class SpecFileModel {
  String fromGroupId
  String fromArtifactId
  String fromVersion
}
```

---

### Phase 5: Documentation & Verification (2-3 hours)

#### 5.1 Create ARCHITECTURE_V7_1.md

Document the design:
- How DependencySpecLoader works
- ClassLoader isolation
- Caching strategy
- No cross-JAR refs limitation

#### 5.2 Update README.md

Add examples:
```xml
<specFile>
  <filePath>specs/api.yml</filePath>
  <fromDependency>
    <groupId>com.company</groupId>
    <artifactId>api-spec-consumidor</artifactId>
  </fromDependency>
  ...
</specFile>
```

#### 5.3 Run full test suite

```bash
mvn -f multiapi-engine/pom.xml clean test
```

**Success Criteria**:
- ✅ All 172+ existing tests pass
- ✅ New dependency loading tests pass
- ✅ No regressions

#### 5.4 Commit Phase 2-5

```bash
git commit -m "Phase 2, 4-5: Integration, Gradle support, and docs for v7.1

- Update SchemaUtil.solveRef() with JAR loader support
- Update OpenApiUtil and MapperPathUtil for dependency resolution
- Wire up OpenApiGenerator for dependency-based loading
- Add Gradle task support (AsyncApiTask, OpenApiTask)
- Complete documentation and architecture notes
- All tests passing (172+ tests)

Fixes #432

Co-Authored-By: Claude Haiku 4.5 <noreply@anthropic.com>"
```

---

## Testing Strategy

### Unit Tests
- DependencySpecLoader: ✅ Complete (7 tests)
- SchemaUtil with JAR: Load from real test JARs
- OpenApiGenerator: Process spec from dependency

### Integration Tests
- Create test JARs with specs
- Use maven-jar-plugin in test pom
- Test producer-consumer pattern

### Regression Tests
- All existing 172+ tests must pass
- No breaking changes

---

## Key Implementation Details

**No cross-JAR refs**: Each JAR is self-contained
- Refs like `$ref: '#/components/...'` work within JAR
- Refs like `$ref: 'other-spec.yml'` work within JAR
- Refs to files in OTHER JARs are NOT supported

**Caching**: URLClassLoaders cached by coordinate
- Map<String, URLClassLoader> in DependencySpecLoader
- Prevents repeated JAR loads
- Cleared on shutdown or explicit call

**Error handling**: Clear, actionable errors
- JAR not found: Clear message with path
- Spec not found in JAR: List what was requested
- Parse errors: Log with context

---

## Success Metrics

✅ Phase 1-3: Core classes + tests (2aa5807)
⏳ Phase 2: Integration (6-8 hours)
⏳ Phase 4: Gradle support (2-3 hours)
⏳ Phase 5: Docs + verification (2-3 hours)

**Total Estimated**: 16-24 hours (14-16 hours remaining)

**Next**: Implement Phase 2 using this guide as specification.
