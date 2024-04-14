import os
import sqlite3
import requests

toshokan_db = os.environ["DATABASE_URL"].removeprefix("sqlite://")
manggregator_db = os.environ["MANGGREGATOR_URL"].removeprefix("sqlite://")


toshokan = sqlite3.connect(toshokan_db)
manggregator = sqlite3.connect(manggregator_db)

# get assets from manggregator
assets: list[tuple[str, str]] = manggregator.execute(
    "SELECT id, name FROM asset WHERE enabled = 1").fetchall()

for id, name in assets:
    print(f'Migrating {name}...')
    toshokan_asset_id, * \
        _ = toshokan.execute(
            f"INSERT INTO assets (title) VALUES ('{name}') RETURNING id"
        ).fetchone()
    toshokan.commit()

    pages: list[tuple[str, str]] = manggregator.execute(
        f"SELECT site, url FROM chapters_page WHERE assetId = '{id}'"
    ).fetchall()

    for site, uri in pages:
        res = requests.post(
            f'http://localhost:8080/asset-scraping/assets/{toshokan_asset_id}/configs',
            json={
                'site': site.capitalize(),
                'isEnabled': True,
                'uri': uri
            }
        )
        try:
            res.raise_for_status()
        except Exception:
            print(res.content)
            raise
    print(f'Done with {name}')

toshokan.close()
manggregator.close()
