# SCS MultiAPI Plugin v7.0 - Unified Response Wrapper Architecture

## Overview

This document describes the comprehensive v7.0 refactoring that eliminates split logic in OpenAPI response wrapper generation and provides a single source of truth for all wrapper decisions.

## Problem Statement (Issue #429)

**Original Issue**: API interfaces referenced non-existent `InlineResponse200*` wrapper classes
- Root cause: Wrapper generation logic was split across two files (OpenApiUtil + MapperPathUtil)
- OpenApiUtil decided which wrappers to CREATE
- MapperPathUtil decided which wrappers to USE
- These decisions were not synchronized, causing references to non-existent classes

**Example**:
```
Response: type: array, items: {$ref: '#/components/schemas/Item'}

OpenApiUtil: "This is array with ref items, skip wrapper"  ❌
MapperPathUtil: "I need InlineResponse200ListItems wrapper"  ❌
Result: API interface references non-existent class
```

## Solution: Unified ResponseWrapperHandler

### Architecture Change

**Before (v6.x)**: Split Logic
```
OpenApiUtil.processResponses()          MapperPathUtil.preparePojoName()
  ↓                                       ↓
  "Create wrappers"        vs           "Use these names"
  (Independent decisions)
  ↓                                       ↓
  Model generation                        API interface generation
  ❌ OUT OF SYNC
```

**After (v7.0)**: Unified Handler
```
ResponseWrapperHandler (Single Source of Truth)
  ├── shouldCreateWrapper(schema)
  ├── getWrapperName(responseCode, operationId, schema)
  ├── extractSchemaForModel(schema)
  ├── getAllWrappers(responseCode, operationId, schema)
  └── [Helper methods for edge cases]
         ↓
    Called by both OpenApiUtil AND MapperPathUtil
         ↓
    ✅ SYNCHRONIZED - Single decision point
```

### Key Methods

#### 1. `shouldCreateWrapper(schema): boolean`
Determines if a response schema needs an inline wrapper class.

**Rules**:
| Case | Creates Wrapper? | Reason |
|------|------------------|--------|
| Inline object | ✅ YES | Need wrapper class |
| Composed type (allOf/anyOf/oneOf) | ✅ YES | Need wrapper class |
| Direct $ref | ❌ NO | Use referenced schema directly |
| Array with ref items | ❌ NO | Generate `List<RefType>` |
| Array with inline items | ✅ YES | Need wrapper for items |
| Array with composed items | ✅ YES | Need wrapper for items |
| Nested array | ✅ YES | Recursive handling |
| Primitive type | ❌ NO | No wrapper needed |
| Null | ❌ NO | Invalid |

#### 2. `getWrapperName(responseCode, operationId, schema): String`
Generates consistent wrapper class names.

**Convention**:
```
InlineResponse{responseCode}{CapitalizedOperationId}{ComposedType}{NestedSuffix}
```

**Examples**:
- Response 200, operation "listItems", inline object
  → `InlineResponse200ListItems`
- Response 200, operation "getUser", allOf composed
  → `InlineResponse200GetUserAllOf`
- Response 201, operation "createOrders", nested array
  → `InlineResponse201CreateOrdersNested`

#### 3. `extractSchemaForModel(schema): JsonNode`
Extracts the actual schema to add to the model map.

**Rules**:
- Inline object → extract the object schema
- Array with inline items → extract ITEMS (not wrapped in array)
- Array with ref items → extract NOTHING (use ref directly)
- Nested array → recurse

#### 4. `getAllWrappers(responseCode, operationId, schema): List<WrapperDefinition>`
Returns all wrapper definitions needed for a schema (handles nested cases).

**Returns**: List of `{name, schema}` pairs to add to model map

## Edge Cases Handled

### Case 1: Array with Ref Items
```json
type: array
items:
  $ref: '#/components/schemas/Item'
```
**Decision**: No wrapper
**Generation**: `List<Item>` (Item is referenced schema)

### Case 2: Array with Inline Object
```json
type: array
items:
  type: object
  properties:
    id: { type: integer }
    name: { type: string }
```
**Decision**: Create wrapper for items
**Generation**: 
- Model: `InlineResponse200ListItems` class
- API: `List<InlineResponse200ListItems>`

### Case 3: Nested Array
```json
type: array
items:
  type: array
  items:
    type: object
```
**Decision**: Create wrapper recursively
**Generation**: `List<List<InlineResponse200ItemsNested>>`

### Case 4: Composed Types (allOf/anyOf/oneOf)
```json
allOf:
  - $ref: '#/components/schemas/Base'
  - $ref: '#/components/schemas/Extended'
```
**Decision**: Create wrapper
**Generation**: `InlineResponse200GetUserAllOf` class

### Case 5: Direct Reference
```json
$ref: '#/components/schemas/User'
```
**Decision**: No wrapper
**Generation**: `User` (referenced schema directly)

## Integration Points

### OpenApiUtil Changes (Model Generation)

**Before**:
```java
private static void processResponses(...) {
  // Complex branching logic
  if (!ApiTool.hasRef(schema) && ApiTool.isObject(schema)) {
    // Create wrapper
  } else if (ApiTool.isComposed(schema)) {
    // Create wrapper
  } else if (ApiTool.isArray(schema)) {
    // Conditional wrapper creation
  }
}
```

**After**:
```java
private static void processResponses(...) {
  // Clean delegation to unified handler
  var wrappers = ResponseWrapperHandler.getAllWrappers(
      response.getKey(), getOperationId(operation), schema, specFile);
  wrappers.forEach(wrapper -> 
    basicJsonNodeMap.put(wrapper.getName(), wrapper.getSchema()));
}
```

### MapperPathUtil Changes (API Interface Generation)

**Before**:
```java
private static String preparePojoName(...) {
  if (ApiTool.isAllOf(schema)) { ... }
  else if (ApiTool.isAnyOf(schema)) { ... }
  else if (ApiTool.hasRef(schema)) { ... }
  else if (ApiTool.isArray(schema)) { ... }  // Incomplete logic
}
```

**After**:
```java
private static String preparePojoName(...) {
  if (ResponseWrapperHandler.shouldCreateWrapper(schema)) {
    return getPojoName(inlineObject, specFile);  // Use wrapper name
  }
  // Otherwise use schema directly
  if (ApiTool.hasRef(schema)) { ... }
  else if (ApiTool.isArray(schema) && ApiTool.hasRef(items)) { ... }
}
```

## Testing Strategy

### Test Coverage (18+ test cases)

1. **Inline Objects** (2 cases)
   - Should create wrapper
   - Schema extraction

2. **Arrays with Ref Items** (2 cases)
   - Should NOT create wrapper
   - No schema extraction

3. **Arrays with Inline Items** (2 cases)
   - Should create wrapper
   - Extract items (not array)

4. **Composed Types** (4 cases)
   - AllOf, AnyOf, OneOf
   - With arrays

5. **Nested Arrays** (3 cases)
   - Inline items
   - Ref items
   - Mixed scenarios

6. **Naming Consistency** (2 cases)
   - Basic naming
   - Composed type suffixes

7. **Comprehensive getAllWrappers** (3 cases)
   - Single wrapper
   - No wrappers
   - Multiple wrappers

8. **Edge Cases** (3 cases)
   - Null schemas
   - Primitive types
   - Invalid inputs

### Regression Testing

- All existing OpenAPI tests continue to pass
- No breaking changes to generated code structure
- Performance not degraded

## Benefits of v7.0 Architecture

### 1. **Single Source of Truth** ✅
- Wrapper decisions made in ONE place (ResponseWrapperHandler)
- Eliminates split logic and synchronization issues
- Easier to verify correctness

### 2. **Maintainability** ✅
- Centralized logic is easier to understand
- Future wrapper types can be added in one place
- Code comments explain decision rules

### 3. **Correctness** ✅
- Issue #429 (missing wrappers) is impossible with unified handler
- All edge cases handled consistently
- Nested arrays properly supported

### 4. **Extensibility** ✅
- New wrapper types can be added by extending ResponseWrapperHandler
- New schema types can be handled by adding cases to shouldCreateWrapper()
- No need to update multiple files

### 5. **Testing** ✅
- Comprehensive test suite (18+ cases)
- All edge cases covered
- Easy to add new test cases

## Migration Notes

### For Users
- No breaking changes to generated code
- v7.0 fixes issue #429 (missing wrappers for array responses)
- Generated API interfaces and models are the same (better organized internally)

### For Contributors
- `ResponseWrapperHandler` is the new central point for wrapper logic
- Don't duplicate wrapper logic in other files
- Refer to `ResponseWrapperHandlerTest` for expected behavior

## Future Improvements (v8.0+)

### Potential Enhancements
1. **AsyncAPI Alignment**: Consider adopting ResponseWrapperHandler for AsyncAPI as well
2. **Wrapper Elimination**: For v8.0+, evaluate if wrapper concept can be eliminated entirely (like AsyncAPI)
3. **Ref Pre-resolution**: Pre-resolve all refs before generation to simplify logic further
4. **Performance Optimization**: Cache wrapper decisions for specs with many responses

## Related Issues
- Issue #429: Missing InlineResponse wrapper classes (v7.0 FIX)
- Issue #391: AsyncAPI architecture (for future alignment)
- Issue #354: Schema pre-processing (for v8.0 consideration)
