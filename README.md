# Zazulak

Railway stations with semaphores and traversal routes modelled with dependent types in Haskell. A project for the Advanced Functional Programming course at MIMUW.

Stations schemas and traversal routes (*de: Fahrstraße*/*pl: droga przebiegu*) are constructed at compilation time. Dependent types forces correct construction of this entities.

In runtime there is simple interface showing station in a schematic way and possibility to switch turnouts and activate/deactivate traversal routes.

## Compiling

```
stack build
```

## Available stations

* Jastarnia, run: `stack exec jastarnia`
* Hel (simplified), run: `stack exec hel`