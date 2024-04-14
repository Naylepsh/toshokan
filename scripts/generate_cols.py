from textwrap import dedent


def template(i: int) -> str:
    generics = ', '.join([f'A{i}' for i in range(1, i + 1)])
    tuple_ = ', '.join([f'Column[A{i}]' for i in range(1, i + 1)])

    return dedent(f"""
      def apply[{generics}](t: ({tuple_})): Columns[({generics})] =
        new Columns(makeFragmentUnsafe(t))""")


for i in range(1, 20):
    print(template(i))
