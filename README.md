Purescript/Halogen v5 -- 2048
---

## Build

- Requires: `npm`, `purescript`, and `spago` OR `nix`
  - Build and serve with locally installed tools

  ```
  npm install
  spago bundle-app --main Main --to assets/index.js
  ./node_modules/.bin/parcel serve assets/index.html
  ```

  - Build and serve with `nix`

  ```
  nix-shell default.nix
  npm install
  spago2nix install
  spago bundle-app --main Main --to assets/index.js
  ./node_modules/.bin/parcel serve assets/index.html
  ```

## Ideas

- Add difficulty setting, harder difficulties have more starting positions
  filled with non-two values

## Notes during development

### Adding a package

If you get something like:

```
[error] The following packages do not exist in your package set:
...
```

First, add a package description to `packages.dhall` under `additions`. Then
add that package name to `spago.dhall`
