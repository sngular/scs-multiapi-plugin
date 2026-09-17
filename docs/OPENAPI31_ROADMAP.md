# OpenAPI 3.1 & JSON Schema 2020-12 Enhancement Roadmap

**Status**: Planning phase
**Derived from**: Audit PR sngular/scs-multiapi-plugin#382
**Repository**: joseegman-idoneea/scs-multiapi-plugin

---

## Executive Summary

Four enhancement items identified from OpenAPI 3.1 and JSON Schema 2020-12 audit. Two are low-effort quick wins; two require architectural changes.

---

## Enhancement #1: Info Metadata (info.summary, license.identifier)

**Status**: ✅ **LOW EFFORT - RECOMMENDED FIRST**
**Issue**: #5
**Effort**: 1.5 hours
**Impact**: LOW (documentation only)

### Problem
`info.summary` (OpenAPI 3.1) and `info.license.identifier` (SPDX) are parsed but ignored.

### Solution
Emit as Javadoc in generated API interface:
```java
/**
 * Pet Store API
 * 
 * <p>Descrição: Manage pets in the store</p>
 * <p>Licença: MIT (SPDX)</p>
 */
public interface PetStoreApi { ... }
```

### Implementation
1. **Parser** (30 min): Read fields in `GlobalObject`/`MapperPathUtil`
2. **Model** (15 min): Add `infoSummary`, `licenseIdentifier` to `CommonSpecFile`
3. **Template** (30 min): Include in Javadoc, update test assets
4. **Testing** (15 min): Verify documentation appears in IDE

### Dependencies
None - independent enhancement.

### Recommendation
**Do this first** - quick win, helps project audit compliance.

---

## Enhancement #2: JSON Schema 2020-12 Array/Object Validation Keywords

**Status**: 📋 **MEDIUM-HIGH EFFORT - PHASED APPROACH RECOMMENDED**
**Issue**: #4
**Effort**: 8-12 hours (phased)
**Impact**: MEDIUM (rarely used in OpenAPI, but growing)

### Problem
Keywords parsed but not validated:
- `contains`, `minContains`, `maxContains` (arrays) ← **HIGH PRIORITY**
- `propertyNames` (object key validation)
- `unevaluatedProperties`, `unevaluatedItems`
- `dependentRequired`, `dependentSchemas`
- `if`/`then`/`else` (conditional schemas)

### Solution
Extend existing Bean Validation infrastructure with custom validators:

```java
// Schema definition:
"inventory": {
  "type": "array",
  "items": { "type": "string" },
  "contains": { "const": "premium" },
  "minContains": 1,
  "maxContains": 10
}

// Generated code:
@ArrayContains(requiredElement = "premium")
@ArrayMinContains(value = 1)
@ArrayMaxContains(value = 10)
private List<String> inventory;
```

### Implementation (3 Phases)

**Phase 1: Array Contains (Hours 1-4)**
1. Parser: Add `contains`, `minContains`, `maxContains` to `SchemaFieldObjectProperties`
2. Parser module: `addPropertiesToFieldObject` in `ModelBuilder`
3. Annotations: Create `@ArrayContains`, `@ArrayMinContains`, `@ArrayMaxContains`
4. Validators: Custom `ArrayContainsValidator`, etc. in templates
5. Template: Include annotations in `templateSchema.ftlh` + test asset

**Phase 2: PropertyNames (Hours 5-8)**
1. Similar flow for `propertyNames` validation
2. Create `@PropertyNamePattern` validator
3. Emit regex-based validation

**Phase 3: Dependent/Unevaluated (Hours 9-12)**
1. More complex - may require cross-field validators
2. Consider postponing to Phase 2 sprint

### Dependencies
- Depends on: Existing Bean Validation infrastructure (no breaking changes)
- Blocks: Nothing else

### Testing Strategy
- Unit tests for parsing each keyword
- Integration tests with generated schemas
- Verify validators enforce constraints at runtime (Spring Bean Validation)

### Recommendation
**Start with Phase 1** (array contains) - most actionable, common pattern.
- Estimate: 4-5 hours
- Impact: Medium (useful for specs with array constraints)
- Complexity: Medium (pattern already established, new validators follow existing style)

---

## Enhancement #3: OpenAPI 3.1 Multi-Type Union Support

**Status**: 🔴 **HIGH EFFORT - ARCHITECTURAL CHANGE REQUIRED**
**Issue**: #3
**Effort**: 16-24 hours
**Impact**: HIGH (common in OpenAPI 3.1)

### Problem

OpenAPI 3.1 allows true union types (different from oneOf/anyOf):
```yaml
type: ["string", "integer"]
```

Current behavior:
- Takes **only first concrete type** → `String`, rest discarded
- `anyOf`/`oneOf` flatten to single POJO with optional fields (no real union)
- No validation that value is one of declared types
- Deserialization can fail when undeclared type arrives

### Solution Options

**Option A: Jackson Custom Deserializer (Recommended)**
```java
// For type: ["string", "integer"]
@JsonDeserialize(using = StringOrIntegerDeserializer.class)
private Object value;
```

**Option B: Sealed Class (Java 17+)**
```java
sealed interface StringOrInteger permits StringValue, IntegerValue {}
record StringValue(String value) implements StringOrInteger {}
record IntegerValue(Integer value) implements StringOrInteger {}
```

**Option C: Wrapper with Type Validation**
```java
private Object value;

@Override
public void setValue(Object val) {
  if (!(val instanceof String || val instanceof Integer)) {
    throw new IllegalArgumentException("Must be String or Integer");
  }
  this.value = val;
}
```

### Implementation Steps

1. **Architecture Decision** (2 hours)
   - Choose option (likely Jackson deserializer for compatibility)
   - Design schema representation for unions

2. **Schema Model** (4 hours)
   - Extend `SchemaFieldObjectType` to represent union type
   - Add `UnionFieldType` class with member types
   - Parser: Detect `type` as array in `ModelBuilder`

3. **Code Generation** (6 hours)
   - Create template for Jackson deserializer (`unionDeserializer.ftlh`)
   - Emit deserializer classes in model package
   - Wire up in schema generation

4. **Testing** (4 hours)
   - Unit tests: Parser correctly identifies unions
   - Integration tests: Deserialization with various types
   - Update asset from `testOpenApi31Union`

5. **Documentation** (2 hours)
   - Update README/wiki with union type explanation
   - Show generated code examples

### Dependencies
- **Blocks**: Nothing, but required for #6 (dynamic refs)
- **Blocked by**: None

### Risk Factors
- Breaking change to generated code (new deserializer classes)
- Compatibility with Jackson 2 vs Jackson 3
- Test asset generation for all union combinations

### Recommendation
**Defer to Phase 2 roadmap** - architectural scope is high.
- Schedule as dedicated sprint item
- Design review recommended before implementation
- Consider impact on Spring Boot 4 (Jackson 3) simultaneously

---

## Enhancement #4: JSON Schema 2020-12 $id/$anchor and $dynamicRef Resolution

**Status**: 🟡 **MEDIUM-HIGH EFFORT - ADVANCED FEATURE**
**Issue**: #6
**Effort**: 12-18 hours
**Impact**: MEDIUM (advanced features, less common in OpenAPI)

### Problem

Reference resolution only by path (`#/components/schemas/Pet`). Advanced JSON Schema features not supported:

```yaml
# $anchor reference not resolved
$ref: "#myanchor"          # references element with $anchor: "myanchor"

# $id base relocation ignored
$id: "https://api.example.com/schema"
$ref: "#/some/path"        # should resolve relative to new base

# Dynamic references not supported
$dynamicRef: "#meta"       # $dynamicAnchor conditional resolution
```

### Solution

**Part 1: $anchor & $id Resolution (Hours 1-8)**

1. **Index phase** (3 hours)
   - During schema collection, build `anchorIndex: {anchorName → Schema}`
   - Track `$id` bases for each schema scope
   - Populate during `SchemaUtil.collectSchemas()`

2. **Resolution phase** (3 hours)
   - Update `SchemaUtil.solveRef()` to:
     - Detect anchor-based refs (`#anchor`)
     - Lookup in `anchorIndex`
     - Apply `$id` base relocation for relative refs

3. **Testing** (2 hours)
   - Unit tests for anchor resolution
   - Integration with external refs
   - Verify base relocation

**Part 2: $dynamicRef Support (Hours 9-12)**

1. **Parser** (2 hours)
   - Recognize `$dynamicRef` and `$dynamicAnchor`

2. **Resolution** (3 hours)
   - Complex - requires scope chain tracking
   - May be simpler to document as "not supported, use $ref" for now

3. **Alternative** (1 hour)
   - Document limitation
   - Provide workaround (convert to `$ref`)

### Implementation Strategy

**Recommended: Two-phase approach**

**Phase 1 (Hours 1-8)**: $anchor & $id support
- High value, medium complexity
- Clear use cases in real specs

**Phase 2 (Later)**: $dynamicRef support
- Lower priority (rarely used)
- Requires deeper architectural changes
- Can be documented as "not yet supported"

### Dependencies
- **Blocks**: None (enhancement only)
- **Blocked by**: #3 (union types) - actually independent

### Interaction with Union Types (#3)
No direct dependency, but both affect `solveRef()` resolution logic.
Coordinate implementation to avoid merge conflicts.

### Testing Strategy
- Unit tests: Each reference type ($anchor, $id-relative, $dynamicRef)
- External reference test suite
- Regression tests with existing path-based refs

### Recommendation
**Schedule for Phase 2 roadmap** - medium complexity.
- Start with $anchor & $id support (clear scope, 8 hours)
- Document $dynamicRef as "not supported" initially
- Revisit $dynamicRef in Phase 3 if needed

---

## Implementation Timeline

### Sprint 1 (This Sprint)

**✅ High Priority - Execute Now**
- [ ] **Issue #5**: Info metadata (1.5 hours)
  - Effort: LOW
  - Impact: LOW but completes audit item
  - Start: This week

### Sprint 2 (2-3 weeks)

**📋 Medium Priority - Plan & Start**
- [ ] **Issue #4 Phase 1**: Array validation keywords (4-5 hours)
  - Effort: MEDIUM
  - Impact: MEDIUM
  - Scope: contains, minContains, maxContains only

- [ ] **Issue #6 Phase 1**: $anchor & $id resolution (6-8 hours)
  - Effort: MEDIUM
  - Impact: MEDIUM
  - Scope: foundation for dynamic resolution

### Sprint 3 (4-6 weeks)

**🔴 Lower Priority - Design Phase**
- [ ] **Issue #3**: Multi-type union support
  - Effort: HIGH (architecture decision required)
  - Impact: HIGH (growing need for 3.1 specs)
  - Action: Design review + prototype

### Future (Post-Sprint 3)

- Issue #4 Phase 2 & 3: Dependent/unevaluated keywords
- Issue #3 Phase 2: Implementation after design approval
- Issue #6 Phase 2: $dynamicRef support

---

## Effort Summary

| Issue | Feature | Phase | Hours | Priority |
|-------|---------|-------|-------|----------|
| #5 | Info metadata | 1 | 1.5 | 🟢 HIGH |
| #4 | Array validation (contains) | 1 | 4-5 | 🟡 MEDIUM |
| #6 | $anchor/$id resolution | 1 | 6-8 | 🟡 MEDIUM |
| #4 | PropertyNames/Dependent/etc | 2 | 4-8 | 🟡 MEDIUM |
| #3 | Union type support | Design | 3-4 | 🟡 MEDIUM |
| #3 | Union type impl | 2-3 | 16-24 | 🔴 HIGH (effort) |
| #6 | $dynamicRef support | 3+ | 4-6 | 🟡 MEDIUM |

---

## Dependencies & Risks

### Critical Path
1. Issue #5 (metadata) - no dependencies, safe first win
2. Issue #4 Phase 1 (array validation) - builds on existing validation infrastructure
3. Issue #6 Phase 1 ($anchor resolution) - foundation for advanced refs
4. Issue #3 (union types) - architectural, high effort but important

### Risks
- **Risk: Breaking changes** - Union types (#3) and validation infrastructure changes may affect existing code generation
  - Mitigation: Semantic versioning, clear changelog, deprecation period

- **Risk: Schema complexity** - JSON Schema 2020-12 is complex; edge cases possible
  - Mitigation: Comprehensive test suite, start with most common patterns

- **Risk: Scope creep** - Can start with small phases and expand
  - Mitigation: Define clear scope per phase, review before expansion

---

## Next Actions

1. **Approve Issue #5 solution** - Post feedback on metadata approach
2. **Assign Issue #5** - Start implementation (1.5 hour task)
3. **Plan Issue #4 Phase 1** - Design validation infrastructure (1-2 hours prep)
4. **Schedule design review for Issue #3** - Scope multi-type union architecture
5. **Create GitHub projects** - Track these items in project board

---

## References

- Audit PR: sngular/scs-multiapi-plugin#382
- OpenAPI 3.1.0 Spec: https://spec.openapis.org/oas/v3.1.0
- JSON Schema 2020-12: https://json-schema.org/specification.html
- Related Issues: #3, #4, #5, #6
