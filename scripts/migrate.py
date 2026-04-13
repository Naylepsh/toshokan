import os
import sqlite3
import re
from itertools import groupby

toshokan_db = os.environ["DATABASE_URL"].removeprefix("sqlite://")


toshokan = sqlite3.connect(toshokan_db)

# asset_ids: list[tuple[int]] = toshokan.execute("""
# SELECT assets.id
# FROM assets
# JOIN categories ON categories.id = assets.category_id
# WHERE categories.name = 'doujinshi'
# """).fetchall()
# ids = [id for id, *_ in asset_ids]
#
# toshokan.execute(
#     f"""
# UPDATE asset_entries
# SET no = '1'
# WHERE asset_id IN ({', '.join('?' for _ in ids)})
# """,
#     ids,
# )
#
# toshokan.commit()


entries: list[tuple[int, str, int]] = toshokan.execute("""
SELECT
  id,
  uri,
  was_seen
FROM
  asset_entries
WHERE
  uri IN (
    SELECT
      uri
    FROM
      asset_entries
    GROUP BY
      uri
    HAVING
      COUNT(*) > 1
  )
ORDER BY
  uri
""").fetchall()


for _, es in groupby(entries, lambda e: e[1]):
    head, *duplicates = sorted(list(es), key=lambda e: -e[2])

    print(f"{head=} ;; {duplicates=}")

    for id, *_ in duplicates:
        toshokan.execute(f"""
        DELETE FROM asset_entries
        WHERE id = {id}
        """)
        toshokan.commit()

#
# for id, uri, was_seen in entries:
#     matches = re.match(r"https://hitomi.la/(.*)/.*-([0-9]+).html", uri)
#     if matches is None:
#         print(f"Failed to match ${uri=}")
#     else:
#         clean_uri = f"https://hitomi.la/{matches[1]}/{matches[2]}"
#         toshokan.execute(f"""
#         UPDATE asset_entries
#         SET uri = '{clean_uri}'
#         WHERE id = {id}
#         """)
#         toshokan.commit()
#
toshokan.close()
