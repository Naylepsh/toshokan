## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

## TODO

- [ ] Add more concurrent handling of scraping jobs
- [ ] Add support for other sites
- [ ] Faulty config pages should be reported somewhere?:
  - Once scraping is done it should display a summary?
- [ ] Add integration with anilist
