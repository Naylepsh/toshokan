-- migrate:up
CREATE TABLE external_chapters (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  kind TEXT NOT NULL,
  path TEXT NOT NULL,
  page_count INTEGER NOT NULL,
  modified_at INTEGER NOT NULL,
  size INTEGER NOT NULL,
  thumbnail_ext TEXT,
  missing_since INTEGER,
  asset_id INTEGER,
  entry_id INTEGER,
  FOREIGN KEY (asset_id) REFERENCES assets (id) ON DELETE SET NULL,
  FOREIGN KEY (entry_id) REFERENCES asset_entries (id) ON DELETE SET NULL
);

-- migrate:down
DROP TABLE external_chapters;
