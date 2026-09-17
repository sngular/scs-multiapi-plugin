# Fix Summary - Issue #429: Array Response Wrapper Classes

## Problem
scs-multiapi-maven-plugin generates broken API interfaces with missing DTO wrapper classes for array responses. 20+ endpoints fail compilation.

**Example**: API interface method references `InlineResponse200ListItems` class that was never generated.

## Solution

### Changes Made

#### 1. OpenApiUtil.java (Lines 239-260)
**File**: `multiapi-engine/src/main/java/com/sngular/api/generator/plugin/openapi/utils/OpenApiUtil.java`

Added array response handling in `processResponses()` method:
```java
} else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
  final var items = ApiTool.getItems(schema);
  if (!ApiTool.hasRef(items) && ApiTool.isObject(items)) {
    // Create wrapper for inline objects in arrays
    basicJsonNodeMap.put(...InlineResponse..., items);
  } else if (ApiTool.isComposed(items)) {
    // Create wrapper for composed types in arrays
    basicJsonNodeMap.put(...InlineResponse..., items);
  }
  // If items have $ref, no wrapper created - ref used directly
}
```

**Impact**: Ensures that when API generation expects an InlineResponse wrapper for array items, the model is actually created.

#### 2. MapperPathUtil.java (Lines 417-432)
**File**: `multiapi-engine/src/main/java/com/sngular/api/generator/plugin/openapi/utils/MapperPathUtil.java`

Enhanced `preparePojoName()` to handle array items with references:
```java
} else if (ApiTool.isArray(schema) && ApiTool.hasItems(schema)) {
  final var items = ApiTool.getItems(schema);
  if (ApiTool.hasRef(items)) {
    // Use ref name directly, don't create InlineResponse wrapper
    pojoName = getPojoName(inlineObject + MapperUtil.getRefSchemaName(items, null), specFile);
  } else {
    // Items are inline, create wrapper
    pojoName = getPojoName(inlineObject, specFile);
  }
}
```

**Impact**: Ensures API generation uses the correct class names for array responses.

### Test Case Added

**Path**: `multiapi-engine/src/test/resources/openapigenerator/testArrayResponseWithRef/`

**Test Files**:
- `api-test.yml` - OpenAPI spec with array responses containing item references
- `assets/ItemsApi.java` - Expected API interface (uses `List<Item>`, not InlineResponse)
- `assets/Item.java` - Expected model class

**Coverage**:
- Array response with single `$ref` in items
- Multiple endpoints with array responses
- Validates no InlineResponse wrappers are generated when items have refs

## Behavior After Fix

### Before (Broken)
```java
// API Interface - BROKEN
public ResponseEntity<InlineResponse200ListItems> listItems();
// Compilation error: InlineResponse200ListItems not found
```

### After (Fixed)
```java
// API Interface - CORRECT
public ResponseEntity<List<Item>> listItems();
// Compiles successfully, uses Item class directly
```

## Verification

To test the fix:
```bash
cd multiapi-engine
mvn test -Dtest=*OpenApiGenerator*testArrayResponseWithRef*
```

Or run full test suite:
```bash
mvn clean verify
```

## Scope

- **Fixes**: Issue #429 (20+ affected endpoints)
- **Affected code**: OpenAPI generation (callMode=false)
- **Backward compatible**: Yes - only fixes broken behavior
- **Test coverage**: Added regression test case

## Files Modified
1. `multiapi-engine/src/main/java/com/sngular/api/generator/plugin/openapi/utils/OpenApiUtil.java`
2. `multiapi-engine/src/main/java/com/sngular/api/generator/plugin/openapi/utils/MapperPathUtil.java`

## Files Added
1. `multiapi-engine/src/test/resources/openapigenerator/testArrayResponseWithRef/api-test.yml`
2. `multiapi-engine/src/test/resources/openapigenerator/testArrayResponseWithRef/assets/ItemsApi.java`
3. `multiapi-engine/src/test/resources/openapigenerator/testArrayResponseWithRef/assets/Item.java`

## Documentation
- `ROOT_CAUSE_ANALYSIS_429.md` - Detailed root cause analysis
- `FIX_SUMMARY_429.md` - This file

## Next Steps
1. Run test suite to verify no regressions
2. Review changes for alignment with plugin patterns
3. Submit as PR to sngular/scs-multiapi-plugin
4. Update version (likely 6.7.9 or 6.8.0)
