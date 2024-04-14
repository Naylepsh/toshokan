## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

## TODO

- [ ] Test that if no entries are found for given config, an error log happens
  - test on `https://mangakakalot.com/manga/bd923417`
  - faulty config pages should be reported somewhere?:
    - Once scraping is done it should display a summary?
- [ ] Add more concurrent handling of scraping jobs
- [ ] Add support for other sites
- [ ] Add integration with anilist
