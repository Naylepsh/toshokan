## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

## TODO

- [ ] Add view for editing schedule
  - [ ] Add route and service & repository methods for updating schedule
- [ ] Add view for category managemet
  - [ ] Add category
  - [ ] Edit category
  - [ ] Delete category
- [ ] Add integration with anilist/mal
- [ ] Increase queries' type-safety:
  - [ ] Add the missing `updateTable` and `-->` from `doobie-typesafe`
  - [ ] Use ^
