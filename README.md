
# What

A simple analog of `multiplate`, made with use of [TypeRepMaps](https://hackage.haskell.org/package/typerep-map-0.3.3.0/docs/Data-TypeRepMap.html).

Provides pretty clean interface to run recurent transformations over your
mutually recursive types.

No template haskell for generating `Descent` instances yet, but it looks easy to do.

# Documentation

Run `stack haddock` to generate documentation.

# Example

See [AST.hs](./app/AST.hs) for example of mut-rec types and [Main.hs](./app/Main.hs) for a transformation that assigns de Brujin index for each variable (`let` is assumed `let rec` there).
