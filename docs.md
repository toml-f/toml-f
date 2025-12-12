Welcome to the API documentation for **TOML Fortran** (toml-f), a TOML parser implementation for data serialization and deserialization in Fortran.

## Getting Started

To use TOML Fortran in your project, import the main module:

```fortran
use tomlf
```

This provides access to all public types and procedures for parsing, manipulating, and serializing TOML data.

## Core Functionality

### Parsing TOML

- [[toml_load]] - Load a TOML document from a file
- [[toml_loads]] - Parse a TOML document from a string
- [[toml_parse]] - Parse TOML from a connected unit

### Serialization

- [[toml_dump]] - Write a TOML table to a file
- [[toml_dumps]] - Serialize a TOML table to a string
- [[toml_serialize]] - Serialize using the visitor pattern

### Data Access

- [[get_value]] - Retrieve values from TOML tables and arrays
- [[set_value]] - Set values in TOML tables and arrays

## Data Types

| Type | Description |
|------|-------------|
| [[toml_table]] | TOML table (key-value mapping) |
| [[toml_array]] | TOML array |
| [[toml_keyval]] | Single key-value pair |
| [[toml_value]] | Abstract base type for all TOML values |
| [[toml_datetime]] | TOML datetime representation |
| [[toml_error]] | Error information from parsing |

## Example

```fortran
use tomlf
implicit none
type(toml_table), allocatable :: table
type(toml_error), allocatable :: error
character(len=:), allocatable :: title

call toml_load(table, "config.toml", error=error)
if (allocated(error)) then
   print '(a)', error%message
   stop 1
end if

call get_value(table, "title", title)
print '(a)', title
```

## Further Resources

- [User Documentation](https://toml-f.readthedocs.io) - Tutorials and how-to guides
- [TOML Specification](https://toml.io/en/v1.0.0) - The TOML v1.0.0 standard
- [GitHub Repository](https://github.com/toml-f/toml-f) - Source code and issue tracker
