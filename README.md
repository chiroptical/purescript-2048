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

### TODO

1. Initial board state is more complicated than two randomly placed 2's
2. Build a 2048 type instead of using `Int`
3. Unit testing, first candidate is the rotation functions
4. Property testing
5. Explore lenses in Purescript
6. Refactor `foldRight` into a more readable state
7. UI Improvements w/ CSS transitions
8. Reset game buttons
9. Win game notification
10. Current Score
11. Possibly Best Score (needs cookies)
