# Modules

- Folders are modules
- Files are modules
- Can make more modules inside files using `mod <name> ... end`
- Modules can be nested

## Imports

- `import Foo.Bar`
  - If `Bar` is a module, then you can then do `Bar.foo()` or `let x: Bar.Submodule.SomeType = ...`
  - If `Bar` is a type, then you can do `let x: Bar = ...`
  - If `Foo` has both a module and a type `Bar`, both get imported
- `import Foo.*` means that everything in `Foo` is now in scope, just like other languages
- `import qualified Foo.Bar` means you have to use `Foo.Bar.foo()` or `let x: Foo.Bar = ...` instead
  of using `Bar` directly
