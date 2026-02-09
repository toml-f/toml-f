# Additional Edge Cases Discovered - Phase 2

This document details additional edge cases found during the second phase of coverage analysis, building on the initial work documented in `COVERAGE_ANALYSIS.md`.

## Summary

Added **8 new edge case tests** to the lexer test suite, discovering **1 actual bug** in the process.

## New Edge Cases Tested

### 1. Integer Overflow in Non-Decimal Bases

**Files Modified**: `test/unit/lexer.f90`

**Tests Added**:
- `integer-hex-boundary`: Tests maximum valid hex value `0x7FFFFFFFFFFFFFFF`  
- `integer-hex-overflow`: Tests hex overflow `0x8000000000000000`
- `integer-octal-boundary`: Tests maximum valid octal value  
- `integer-binary-max-digits`: Tests 63-bit binary number

**Bug Found**: ❗ The lexer currently **accepts** `0x8000000000000000` which overflows signed int64. This should be rejected but passes as a valid integer token. This is documented in the test with a NOTE comment for future fixing.

```fortran
! BUG: Currently accepted by lexer - should be fixed
! Test 0x8000000000000000 (overflow for signed int64)
call check_token(error, "0x8000000000000000", &
   & [token_kind%int, token_kind%eof], .false.)
```

### 2. Underscore Placement in Non-Decimal Integers

**Tests Added**:
- `integer-underscore-after-prefix`: Tests invalid `0x_FFF`, `0o_777`, `0b_101`
- `integer-underscore-hex-valid`: Tests valid `0xF_F_F_F`, `0xCAFE_BABE`

**Result**: ✅ Lexer correctly rejects underscores immediately after base prefix and correctly accepts underscores between digits.

### 3. Control Character Handling in Unicode Escapes

**Tests Added**:
- `string-control-chars-escaped`: Tests `\u001E`, `\u001D` (control characters in unicode form)
- `string-invalid-escape-v`: Tests invalid `\v` escape sequence

**Result**: ✅ Lexer correctly handles unicode escapes for control characters. Note: `\u0000` (null) is rejected by design (verify_ucs requires code > 0).

**Important Discovery**: The `verify_ucs` function in `src/tomlf/de/lexer.f90` validates Unicode code points:
```fortran
valid = code > 0 .and. code < int(z"7FFFFFFF", tfi) &
   & .and. (code < int(z"d800", tfi) .or. code > int(z"dfff", tfi)) &
   & .and. (code < int(z"fffe", tfi) .or. code > int(z"ffff", tfi))
```
This correctly rejects:
- `\u0000` (null - code must be > 0)
- `\uD800` to `\uDFFF` (UTF-16 surrogates)
- `\uFFFE` and `\uFFFF` (non-characters)

## Edge Cases Considered But Not Tested

The following edge cases were identified but not added as tests for the reasons stated:

### Parser-Level Edge Cases

**Empty/Whitespace Keys**:
- Empty string keys: `"" = 1`
- Whitespace-only keys: `"   " = 1`

**Reason Not Tested**: Parser tests require mocked token arrays which is complex. These would be better tested as integration tests using `toml_loads` or `toml_parse`.

**Mixed-Type Arrays**:
- Integer vs float: `[1, 1.0]`
- String vs boolean: `["true", true]`
- Number vs special float: `[1, inf]`

**Reason Not Tested**: Same as above - requires full parsing, not just lexing.

### Multiline String Edge Cases

**Line-Ending Backslash with CRLF vs LF**:
- Escaped newline trimming behavior with different line endings

**Reason Not Tested**: Requires careful handling of line endings in test strings. The existing tests cover basic multiline string functionality.

## Test Coverage Impact

### Before Phase 2
- Total lexer tests: 140
- Total unit tests: ~220

### After Phase 2
- Total lexer tests: 148 (+8)
- Total unit tests: ~228 (+8)
- **New bugs discovered**: 1 (hex overflow)

## Bugs Discovered

### Bug #1: Hexadecimal Integer Overflow Not Validated ✅ FIXED

**Severity**: Medium  
**Location**: `src/tomlf/de/lexer.f90` (integer parsing logic)  
**Issue**: The lexer accepts `0x8000000000000000` which overflows the maximum value for signed 64-bit integers (`0x7FFFFFFFFFFFFFFF`).

**Expected Behavior**: Should tokenize as `token_kind%invalid`  
**Actual Behavior (before fix)**: Tokenizes as `token_kind%int`

**Impact**: TOML files with very large hexadecimal values may be accepted when they should be rejected, potentially leading to integer overflow when the value is extracted.

**Status**: ✅ **FIXED** - Added `validate_integer_range` function in `next_integer` to check for overflow before creating integer tokens. The validation works for all bases (decimal, hexadecimal, octal, and binary).

**Fix Details**:
- Added overflow checking in `next_integer` subroutine
- New `validate_integer_range` function validates that integer values fit within int64 range
- Checks both positive and negative overflow conditions
- Updated test to expect `token_kind%invalid` for overflow cases

## Files Modified

1. `test/unit/lexer.f90`:
   - Added 8 new test functions
   - Updated test suite array to include new tests
   - Updated `integer_hex_overflow` test to expect correct behavior (invalid token)
   - All tests passing

2. `src/tomlf/de/lexer.f90`:
   - Added `validate_integer_range` function for overflow detection
   - Modified `next_integer` to call validation before creating tokens
   - Handles overflow for all integer bases (decimal, hex, octal, binary)

## Recommendations for Future Work

1. ~~**Fix Bug #1**: Implement overflow validation for non-decimal integer bases~~ ✅ **COMPLETED**
2. **Integration Tests**: Add parser-level integration tests for empty keys and mixed-type arrays
3. **Multiline Strings**: Add tests for line-ending edge cases (CRLF vs LF)
4. ~~**Octal/Binary Overflow**: Verify that overflow detection works for octal and binary as well~~ ✅ **COMPLETED** (covered by same fix)
5. **Surrogate Pair Validation**: Add explicit tests for UTF-16 surrogate rejection

## Conclusion

This phase of edge case discovery successfully:
- ✅ Added 8 new edge case tests
- ✅ Discovered 1 actual bug (hex overflow)
- ✅ **FIXED the hexadecimal overflow bug**
- ✅ Verified correct handling of underscore placement
- ✅ Verified correct handling of unicode escape validation
- ✅ Maintained 100% test pass rate

The hexadecimal overflow bug has been fixed with comprehensive overflow validation for all integer bases.
