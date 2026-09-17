# Issue #429 - Recommendations & Action Plan

## Summary

The current fix is **acceptable as a hotfix** but requires follow-up work to handle edge cases and improve maintainability.

---

## Immediate Actions (Before Deploy)

### 1. Add Code Comments (15 min)
**Location**: `OpenApiUtil.java:255` and `MapperPathUtil.java:427`

```java
// OpenApiUtil.java - Add comment at line 255
} else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
  // Handle array responses: extract items and check if wrapper needed
  // Issue #429: Array items with direct $ref should use ref directly,
  // while inline objects/composed types need wrapper for model generation.
  // Note: For consistency, MapperPathUtil.preparePojoName() mirrors this logic.
  // TODO: Consolidate this split logic into unified handler (Issue #XYZ)
  final var items = ApiTool.getItems(schema);
  if (!ApiTool.hasRef(items) && ApiTool.isObject(items)) {
    // Inline object needs wrapper
    basicJsonNodeMap.put(...);
  } else if (ApiTool.isComposed(items)) {
    // Composed items need wrapper
    basicJsonNodeMap.put(...);
  }
  // Arrays with refs or primitives: no wrapper needed
}

// MapperPathUtil.java - Add comment at line 427
} else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
  // Mirror logic from OpenApiUtil.processResponses()
  // Extract items and use ref directly if available, otherwise use inline wrapper
  final var items = ApiTool.getItems(schema);
  if (ApiTool.hasRef(items)) {
    // Use ref directly - no InlineResponse wrapper
    pojoName = getPojoName(inlineObject + MapperUtil.getRefSchemaName(items, null), specFile);
  } else {
    // Use inline wrapper
    pojoName = getPojoName(inlineObject, specFile);
  }
}
```

### 2. Add Test Case for Nested Arrays (30 min)
**Location**: `multiapi-engine/src/test/java/com/sngular/api/generator/plugin/openapi/.../OpenApiGeneratorTest.java`

```java
@Test
void testNestedArrayResponse() {
  // GIVEN: OpenAPI spec with nested array response
  final JsonNode spec = """
    openapi: 3.0.0
    info: { title: Test, version: 1 }
    paths:
      /items:
        get:
          operationId: getNestedItems
          responses:
            '200':
              description: Nested array
              content:
                application/json:
                  schema:
                    type: array
                    items:
                      type: array
                      items:
                        type: string
    """;
  
  // WHEN: Generated
  final var result = generator.generate(spec);
  
  // THEN: API method returns List<List<String>>
  final var apiMethod = result.getApiInterface().getMethod("getNestedItems");
  assertThat(apiMethod.getReturnType()).isEqualTo("List<List<String>>");
  
  // No wrapper classes should be generated
  assertThat(result.getModelClasses()).noneMatch(c -> c.getName().contains("InlineResponse"));
}

@Test
@Disabled("Issue #XYZ: Nested arrays with refs need handler")
void testNestedArrayWithRefResponse() {
  // Currently generates unspecified List type
  // Should generate List<List<Product>>
}
```

### 3. Document Limitation in README (10 min)
**Add to**: `docs/KNOWN_LIMITATIONS.md` (new file)

```markdown
# Known Limitations

## Issue #429: Array Response Handling

### Fixed in v6.7.x
- Array responses with direct `$ref` items
- Array responses with inline objects
- Array responses with composed items (anyOf/oneOf/allOf)

### Not Yet Supported
- **Nested arrays** (2+ levels deep): May generate unspecified `List` type
  ```yaml
  type: array
  items:
    type: array
    items: {$ref: '#/components/schemas/Item'}
  ```
  **Workaround**: Flatten the nested array to a single type in your spec

- **Array with $ref inside composed**: 
  ```yaml
  type: array
  items:
    anyOf:
      - $ref: '#/components/schemas/A'
      - type: object
  ```
  **Workaround**: Extract inner types to separate schemas

### Timeline for Full Support
- Target: v6.8 (nested array support)
- Full refactor: v7.0 (unified wrapper handler)
```

---

## Short-term Actions (Next 1-2 Weeks)

### 4. Add Nested Array Support

**Effort**: 2-4 hours
**Priority**: HIGH (fixes critical edge case)

**Implementation**:
```java
// OpenApiUtil.java - Add helper method
private static JsonNode extractArrayItemType(JsonNode schema) {
  if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
    return extractArrayItemType(ApiTool.getItems(schema));
  }
  return schema;
}

// Update processResponses() to use it
} else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
  final var items = extractArrayItemType(schema);  // Extract innermost type
  if (!ApiTool.hasRef(items) && ApiTool.isObject(items)) {
    basicJsonNodeMap.put(...);
  } else if (ApiTool.isComposed(items)) {
    basicJsonNodeMap.put(...);
  }
}

// MapperPathUtil.java - Mirror change
} else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
  final var items = extractArrayItemType(schema);  // Extract innermost type
  if (ApiTool.hasRef(items)) {
    pojoName = getPojoName(inlineObject + MapperUtil.getRefSchemaName(items, null), specFile);
  } else {
    pojoName = getPojoName(inlineObject, specFile);
  }
}
```

**Testing**:
- Add `testNestedArrayWithRef` (currently @Disabled)
- Add `testTripleNestedArray`
- Verify correct generic type generation

**PR Title**: 
```
Fix #XYZ: Support nested arrays in OpenAPI response generation

- Extract innermost item type for multi-level array responses
- Correctly generates List<List<Item>> instead of unspecified List
- Adds comprehensive test coverage for nested arrays
```

### 5. Enhance Test Coverage

**Add test cases** for edge cases:

```java
@Test
void testArrayResponseWithInlineObject() {
  // Array of inline objects
  spec = """
    schema:
      type: array
      items:
        type: object
        properties:
          id: {type: string}
    """;
  result = generator.generate(spec);
  // Should create InlineResponse200GetArray wrapper
  assertThat(result.getModelClasses()).anyMatch(c -> c.getName().contains("InlineResponse200GetArray"));
}

@Test
void testArrayResponseWithComposedItems() {
  // Array of anyOf/oneOf/allOf items
  spec = """
    schema:
      type: array
      items:
        anyOf:
          - $ref: '#/components/schemas/A'
          - $ref: '#/components/schemas/B'
    """;
  result = generator.generate(spec);
  // Should create InlineResponse200GetArrayAnyOf wrapper
  assertThat(result.getModelClasses()).anyMatch(c -> c.getName().contains("AnyOf"));
}

@Test
void testArrayResponseWithPrimitiveItems() {
  // Array of primitives
  spec = """
    schema:
      type: array
      items: {type: string}
    """;
  result = generator.generate(spec);
  // Should NOT create wrapper - direct List<String>
  assertThat(result.getModelClasses()).noneMatch(c -> c.getName().contains("InlineResponse"));
}

@Test
void testMixedResponseTypes() {
  // Same endpoint with array and non-array responses
  spec = """
    paths:
      /items:
        get:
          responses:
            '200':
              content:
                application/json:
                  schema:
                    type: array
                    items: {$ref: '#/components/schemas/Item'}
            '400':
              content:
                application/json:
                  schema:
                    type: object
                    properties:
                      error: {type: string}
    """;
  result = generator.generate(spec);
  // 200 should use List<Item>, 400 should use error object
  assertThat(result.getApiInterface().getMethod("getItems")).returns("List<Item>");
}
```

**Total new tests**: 8-10 cases
**Effort**: 2-3 hours

### 6. Verify No Regressions

**Run full test suite**:
```bash
mvn clean test -f multiapi-engine/pom.xml
mvn clean test -f scs-multiapi-maven-plugin/pom.xml
mvn clean test -f scs-multiapi-gradle-plugin/build.gradle
```

**Expected**:
- All existing tests pass
- New tests for nested arrays pass
- No performance regression

---

## Medium-term Actions (Next Sprint - 2-3 Weeks)

### 7. Plan Refactoring (Alternative A: Unified Handler)

**Effort**: 8-16 hours (with thorough testing)
**Priority**: MEDIUM (improves maintainability)

**Scope**:
1. Create `ResponseSchemaResolution` value object
2. Extract logic from both files into single method
3. Update both callers to use new method
4. Comprehensive testing (20+ test cases)
5. Migration guide for any public API changes

**Deliverables**:
- Refactored code
- 20+ test cases
- Cleanup of duplicated logic
- Performance verification

**PR Template**:
```
Refactor: Unify array/object response wrapper logic

This refactoring consolidates response schema resolution logic 
that was split between OpenApiUtil and MapperPathUtil into a 
single method, improving maintainability and extensibility.

Fixes: Makes it easier to add support for new cases
Relates to: #429, Future refactoring

Changes:
- Extract ResponseSchemaResolution logic into single method
- Remove duplication between OpenApiUtil and MapperPathUtil
- Add 15+ test cases for edge cases
- No functional changes to generated code

Testing: All existing tests pass + 15 new cases
```

### 8. Document Architecture Decision

**Create**: `docs/RESPONSE_WRAPPER_STRATEGY.md`

```markdown
# Response Wrapper Strategy

## Why OpenAPI Has Wrappers (AsyncAPI Doesn't)

### OpenAPI Approach
- REST APIs need named types for response schemas
- Anonymous schemas need generated wrapper classes
- Example: Array of inline objects → InlineResponse200 wrapper

### AsyncAPI Approach  
- Message payloads are used directly
- No need for wrapper classes
- Simpler, flatter model hierarchy

## When Wrappers Are Created

| Scenario | Wrapper Created | Why |
|----------|-----------------|-----|
| Array with $ref | NO | Use ref directly (e.g., List<Product>) |
| Array with inline object | YES | Need model for object (InlineResponse200) |
| Array with anyOf/oneOf/allOf | YES | Need composed model (InlineResponse200AnyOf) |
| Array with primitives | NO | Use primitive (List<String>) |
| Single object with $ref | NO | Use ref directly (Product) |
| Single inline object | YES | Need wrapper (InlineResponse200) |

## Future Direction

See Deprecation Plan in issue #XYZ for migration towards AsyncAPI-like approach.
```

---

## Long-term Actions (Next Quarter)

### 9. Plan v7.0 Refactoring (Alternative B: Deprecation)

**Decision needed**: 
- Continue with unified handler approach (Alternative A), OR
- Start deprecation journey towards AsyncAPI model (Alternative B)

**If Alternative A**:
- Plan 1-2 sprint refactor
- Design migration if any public APIs change
- Schedule for v7.0

**If Alternative B** (RECOMMENDED):
1. **v6.8** (current): 
   - Add nested array support
   - Add deprecation warning for wrapper generation
   - Introduce `useDirectItemsForArrays` flag

2. **v7.0** (next major):
   - Make `useDirectItemsForArrays=true` default
   - Old wrapper behavior behind flag
   - Migration guide for users

3. **v8.0** (future):
   - Remove wrapper support entirely
   - Align with AsyncAPI

**Effort**: Design phase 4 hours, implementation spreads across 3 releases

---

## Summary of Recommendations

| Action | Timeline | Effort | Priority | Impact |
|--------|----------|--------|----------|--------|
| Add comments | Before deploy | 15 min | HIGH | Medium |
| Add test (nested) | Before deploy | 30 min | HIGH | High |
| Document limitation | Before deploy | 10 min | HIGH | Medium |
| Add nested array support | Week 2 | 4 hours | HIGH | High |
| Enhance test coverage | Week 2 | 3 hours | MEDIUM | High |
| Plan refactoring | Sprint 2 | 2 hours | MEDIUM | Medium |
| Implement refactor | Sprint 2 | 16 hours | MEDIUM | High |
| Plan v7.0 strategy | Sprint 2 | 4 hours | MEDIUM | High |
| Implement deprecation | v6.8-v8.0 | 24 hours | LOW | High |

---

## Critical Path (Must Do)

```
[Now] Deploy hotfix + comments + test
  ↓
[Week 2] Add nested array support
  ↓
[Sprint 2] Plan and execute refactoring OR deprecation
  ↓
[v7.0] Implement long-term architecture
```

## Success Metrics

- ✅ Issue #429 fixed (20+ endpoints working)
- ✅ Nested arrays supported in v6.8
- ✅ Unified handler implemented in v7.0
- ✅ Zero regressions throughout

---

## Questions for Team

1. **Should we pursue Alternative A (refactoring) or Alternative B (deprecation)?**
2. **Is nested array support critical for v6.8, or can it wait?**
3. **Should we plan v7.0 roadmap now or after v6.8 release?**
4. **Should we add AsyncAPI alignment as a long-term goal?**
