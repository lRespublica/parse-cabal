# Description

The parse-cabal utility is a tool designed to retrieve information from .cabal files.
It is primarily intended for use in shell scripts.

The available output formats are plain text, TOML and JSON.

# Help

```
    Parse a cabal file
    
    Usage: parse-cabal [--to plain json toml]
                       [(-f|--file FILENAME) | --from-current-dir]
                       all name version pkgid license description synopsis homepage builddeps executables
    
    Available options:
      --to plain json toml     Output style (default: Plain)
      -f,--file FILENAME       Read data from specified cabal file
      --from-current-dir       (default)
      -h,--help                Show this help text
```
