## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

## TODO

- Instead of having one form for both asset and its configs, do the following:
    - New asset form has only the asset fields
    - Upon successful creation it redirects to edit form
    - Edit form still has the form for the asset, but also "subforms" for each of the configs, like so:
        ```
        Uri: [...] Site: [foo/bar/baz] Enabled [x] [+][- (remove from form)]
        Uri: [...] Site: [foo/bar/baz] Enabled [x] [+][del (delete from db)]
        ```
