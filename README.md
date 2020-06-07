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

- [ ] Make initial board state more complicated than two randomly placed 2's
- [ ] Use a 2048 type instead of using `Int`
- [ ] Explore lenses in Purescript
- [ ] Refactor `foldRight` into a more readable state
- [ ] UI improvements w/ CSS transitions
- [ ] Reset game button and keystroke
- [ ] Win game notification
- [ ] Current score
- [ ] Add continuous integration with Github actions?
- [ ] Add unit/property tests for pure code
- [ ] Add unit/property tests for the DOM
- [ ] Refactor data types from Board/Component.purs
- [ ] Add ability to save current game and high score via cookies or localstorage?
