## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

## TODO

- [ ] Add integration with anilist/mal
- [ ] Split `EntryNo` into `EntryTitle` and `EntryNo`. `EntryNo` could default to `1` in case of parsing failure. `EntryTitle` could default to `None` in case of failure?
- [ ] Add view for category management
  - [ ] Add category
  - [ ] Edit category
  - [ ] Delete category
