## Using TOML-Fortran with the Fortran package manager

The Fortran package manager [`fpm`](https://github.com/fortran-lang/fpm) can be used to declare TOML-Fortran as dependency for a project, just add

```toml
[dependencies]
toml-f = { git = "https://github.com/toml-f/toml-f" }
```

For more details checkout the packaging information in the `fpm` repository.

This example project will build a simple executable to read and emit TOML documents using the TOML-Fortran dependency provided from the project root.
Note that dirty worktrees cannot be tested as `fpm` will clone the current head of the worktree as dependency.
