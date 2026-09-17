# Root Cause Analysis - Issue #429

## Issue Summary
scs-multiapi-maven-plugin generates API interfaces that reference non-existent `InlineResponse200*` wrapper classes for array responses, causing compilation failures.

**Affected**: 20+ endpoints with array responses (e.g., `Listar*`, `Consultar*PorEstado`)
**Impact**: Critical - generated code does not compile
**Scope**: OpenAPI generation with `callMode=false` (API server mode)

---

## Root Cause

### The Problem Flow

#### 1. **Model Generation Phase** (OpenApiUtil.processResponses)
**File**: `multiapi-engine/src/main/java/com/sngular/api/generator/plugin/openapi/utils/OpenApiUtil.java:239-260`

```java
private static void processResponses(Map<String, JsonNode> basicJsonNodeMap, JsonNode operation, SpecFile specFile) {
  // Lines 247-255
  if (!ApiTool.hasRef(schema) && ApiTool.isObject(schema)) {
    // Create InlineResponse wrapper for inline objects
    basicJsonNodeMap.put(...InlineResponse..., schema);
  } else if (ApiTool.isComposed(schema)) {
    // Create InlineResponse wrapper for composed types
    basicJsonNodeMap.put(...InlineResponse..., schema);
  }
  // MISSING: No handling for array responses!
}
```

**Current behavior**:
- Inline objects (no $ref, type=object) → Create InlineResponse wrapper ✓
- Composed types (allOf/anyOf/oneOf) → Create InlineResponse wrapper ✓
- **Array responses** (type=array, items=$ref) → **NOT handled** ✗

**For array response** like `type: array, items: {$ref: '#/components/schemas/Dto'}`:
- `!ApiTool.hasRef(schema)` = true (array itself has no $ref)
- `ApiTool.isObject(schema)` = false (array is not object type)
- **Result**: Condition fails, NO wrapper model added to schemaMap

#### 2. **API Interface Generation Phase** (MapperPathUtil)
**File**: `multiapi-engine/src/main/java/com/sngular/api/generator/plugin/openapi/utils/MapperPathUtil.java:391-432`

```java
private static List<ContentObject> mapContentObject(
    SpecFile specFile, JsonNode content, String inlineObject, GlobalObject globalObject, Path baseDir) {
  // Line 398: Gets schema from content
  final var schema = ApiTool.getNode(ApiTool.getNode(content, mediaType), SCHEMA);
  
  // Line 399: Call preparePojoName with inlineObject="InlineResponse200OperationIdCap"
  final String pojoName = preparePojoName(inlineObject, schema, specFile);
  
  // For array response with items.$ref:
  // preparePojoName returns: "InlineResponse200..." (doesn't detect it should use ref name)
}

private static String preparePojoName(String inlineObject, JsonNode schema, SpecFile specFile) {
  // Lines 419-429
  if (ApiTool.isAllOf(schema)) { ... }
  else if (ApiTool.isAnyOf(schema)) { ... }
  else if (ApiTool.isOneOf(schema)) { ... }
  else if (ApiTool.hasRef(schema)) {
    // Use the ref name directly (but array doesn't have direct ref!)
    pojoName = getPojoName(inlineObject + MapperUtil.getRefSchemaName(schema, null), specFile);
  } else {
    // Fallback: Create InlineResponse wrapper name
    pojoName = getPojoName(inlineObject, specFile);  // "InlineResponse200..."
  }
  return pojoName;
}
```

**The mismatch**:
- `MapperPathUtil` generates API interface method that references `InlineResponse200...` class
- `OpenApiUtil` never added `InlineResponse200...` to the model schemaMap
- **Result**: API tries to use a class that was never generated → compilation error

---

## Detailed Example

### Input: Array Response in OpenAPI Spec
```yaml
paths:
  /api/v1/listItems:
    get:
      operationId: listItems
      responses:
        '200':
          description: List of items
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/Item'
components:
  schemas:
    Item:
      type: object
      properties:
        id: { type: string }
        name: { type: string }
```

### What Currently Happens

**Step 1** (OpenApiUtil.processResponses):
```
Schema: { type: "array", items: { $ref: "#/components/schemas/Item" } }
- !hasRef(schema) = true ✓
- isObject(schema) = false ✗
- Condition FAILS → No model added
```

**Step 2** (MapperPathUtil.mapContentObject):
```
inlineObject = "InlineResponse200ListItems"
schema = { type: "array", items: { $ref: "#/components/schemas/Item" } }
pojoName = preparePojoName(inlineObject, schema, specFile)
  - hasRef(schema) = false (array has no direct ref!)
  - Returns: "InlineResponse200ListItems"
```

**Step 3** (Generated API Interface):
```java
// Generated in ApiInterface.java
public List<InlineResponse200ListItems> listItems();
```

**Step 4** (Compilation):
```
ERROR: Cannot find symbol: class InlineResponse200ListItems
       (Never generated because OpenApiUtil didn't add it to schemaMap)
```

---

## Root Cause Summary

| Phase | File | Issue |
|-------|------|-------|
| **Model Gen** | OpenApiUtil.java:239-260 | Missing array response handling in `processResponses()` |
| **API Gen** | MapperPathUtil.java:417-432 | `preparePojoName()` doesn't detect array with items.$ref pattern |
| **Sync Gap** | Both files | Model generation doesn't match what API generation expects |

---

## The Fix Required

### Option 1: Add Array Handling to OpenApiUtil (Recommended)

In `OpenApiUtil.processResponses()` after line 255, add:

```java
} else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
  final var items = ApiTool.getItems(schema);
  // Only create wrapper if items are inline (not refs)
  if (!ApiTool.hasRef(items) && ApiTool.isObject(items)) {
    basicJsonNodeMap.put(
        StringCaseUtils.titleToSnakeCase(MapperUtil.getPojoName(
            "InlineResponse" + response.getKey() + StringUtils.capitalize(getOperationId(operation)), specFile)),
        items);
  } else if (ApiTool.isComposed(items)) {
    basicJsonNodeMap.put(
        StringCaseUtils.titleToSnakeCase(MapperUtil.getPojoName(
            "InlineResponse" + response.getKey() + StringUtils.capitalize(getOperationId(operation)) + 
            getComposedJsonNodeName(items), specFile)),
        items);
  }
  // If items have $ref, no wrapper needed - the ref will be used directly
}
```

### Option 2: Improve Array Handling in MapperPathUtil

In `preparePojoName()` after line 424, add before the `hasRef` check:

```java
} else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
  final var items = ApiTool.getItems(schema);
  if (ApiTool.hasRef(items)) {
    // Use the ref name, don't create wrapper
    pojoName = getPojoName(inlineObject + MapperUtil.getRefSchemaName(items, null), specFile);
  } else {
    // Items are inline, create wrapper
    pojoName = getPojoName(inlineObject, specFile);
  }
}
```

---

## Recommendation

**Implement Option 1** (OpenApiUtil fix):
- More straightforward - ensures models are generated when needed
- Aligns with existing pattern (similar to composed types)
- Handles both cases: inline array elements and array of refs
- No API generation changes needed

---

## Test Case Needed

Create test: `testArrayResponseWithRef`
```yaml
responses:
  '200':
    description: Array of items
    content:
      application/json:
        schema:
          type: array
          items:
            $ref: '#/components/schemas/Item'
```

Expected output:
- API method: `List<Item> listItems()`  (NOT InlineResponse200...)
- No InlineResponse wrapper generated
- Item model is used directly
- Code compiles without errors

---

## Files Affected
- `multiapi-engine/src/main/java/com/sngular/api/generator/plugin/openapi/utils/OpenApiUtil.java`
- Possibly: `multiapi-engine/src/test/java/.../OpenApiGeneratorFixtures.java` (for new test case)
