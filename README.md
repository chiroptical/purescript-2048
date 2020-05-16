Purescript/Halogen v5 -- 2048
---

- Build/Bundle/Watch: `spago bundle-app -w -m Main`
- Serve: `parcel serve assets/index.html -o index--parcelified.html`

### Adding a package

If you get something like:

```
[error] The following packages do not exist in your package set:
...
```

First, add a package description to `packages.dhall` under `additions`. Then
add that package name to `spago.dhall`

### Next steps

1. Actually handle keyboard inputs and update state
2. Initial board state should include 4's as a low probability
3. Build a 2048 type instead of using Integer
