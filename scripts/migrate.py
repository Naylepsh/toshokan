import os
import sqlite3

toshokan_db = os.environ["DATABASE_URL"].removeprefix("sqlite://")


toshokan = sqlite3.connect(toshokan_db)

asset_ids: list[tuple[int]] = toshokan.execute("""
SELECT assets.id
FROM assets
JOIN categories ON categories.id = assets.category_id
WHERE categories.name = 'doujinshi'
""").fetchall()
ids = [id for id, *_ in asset_ids]

toshokan.execute(f"""
UPDATE asset_entries
SET no = '1'
WHERE asset_id IN ({', '.join('?' for _ in ids)})
""", ids)

toshokan.commit()

# for id, name in assets:
#     print(f'Migrating {name}...')
#     toshokan_asset_id, * \
#         _ = toshokan.execute(
#             f"INSERT INTO assets (title) VALUES ('{name}') RETURNING id"
#         ).fetchone()
#     toshokan.commit()
#
#     print(f'Done with {name}')
#
toshokan.close()
