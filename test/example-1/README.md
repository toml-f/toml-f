## Using TOML-Fortran with the Fortran package manager

The Fortran package manager [`fpm`](https://github.com/fortran-lang/fpm) can be used to declare TOML-Fortran as dependency for a project, just add

```toml
[dependencies]
toml-f = { git = "https://github.com/toml-f/toml-f" }
```

For more details checkout the packaging information in the `fpm` repository.

This example project will build a simple executable to read and process TOML documents using the TOML-Fortran dependency provided from the project root, serving both as example and a simple integration test.
Note that dirty worktrees cannot be tested as `fpm` will clone the current head of the worktree as dependency, once you update the root project make sure to delete the `fpm` build directory to force it to reload the TOML-Fortran dependency.

Test this example by running it from `fpm` on one of the provided example TOML documents:

```
fpm run --args package-1.toml
fpm run --args package-2.toml
```

Note, this is an example project with a made-up package format, for the mere reason of demonstrating functionality, therefore, do not expect all design choices in this example to be sensible ones.
