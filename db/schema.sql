CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE assets (
    id INTEGER PRIMARY KEY,
    title TEXT NOT NULL UNIQUE
, category_id INTEGER
    REFERENCES categories (id));
CREATE TABLE asset_entries (
    id INTEGER PRIMARY KEY,
    no TEXT NOT NULL,
    uri TEXT NOT NULL,
    date_uploaded TEXT NOT NULL,
    asset_id INTEGER NOT NULL, title TEXT NOT NULL DEFAULT '',

    CONSTRAINT fk_asset_id
        FOREIGN KEY (asset_id)
        REFERENCES assets (id)
        ON DELETE CASCADE
);
CREATE TABLE asset_scraping_configs (
    id INTEGER PRIMARY KEY,
    uri TEXT NOT NULL,
    site TEXT NOT NULL,
    is_enabled INTEGER NOT NULL DEFAULT 0,
    asset_id INTEGER NOT NULL,

    CONSTRAINT fk_asset_id
        FOREIGN KEY (asset_id)
        REFERENCES assets (id)
        ON DELETE CASCADE
);
CREATE TABLE categories (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);
CREATE TABLE scraping_schedules (
    id INTEGER PRIMARY KEY,
    category_id INTEGER NOT NULL,
    day_of_week INTEGER NOT NULL,

    CONSTRAINT fk_category_id
        FOREIGN KEY (category_id)
        REFERENCES categories (id)
        ON DELETE CASCADE
);
CREATE TABLE tokens (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    value TEXT NOT NULL UNIQUE,
    expires_at INTEGER NOT NULL
);
CREATE TABLE mal_manga_mapping (
    id INTEGER PRIMARY KEY,
    manga_id INTEGER NOT NULL UNIQUE,
    mal_id INTEGER NOT NULL UNIQUE,

    CONSTRAINT fk_manga_id
        FOREIGN KEY (manga_id)
        REFERENCES assets (id)
        ON DELETE CASCADE
);
CREATE TABLE entry_progress (
    entry_id INTEGER PRIMARY KEY,
    was_seen BOOLEAN NOT NULL DEFAULT FALSE,
    date_marked_seen DATETIME,
    FOREIGN KEY (entry_id) REFERENCES asset_entries(id) ON DELETE CASCADE
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20240130211545'),
  ('20240524194511'),
  ('20240529124324'),
  ('20240529175125'),
  ('20240712124302'),
  ('20240824174343'),
  ('20240925081137'),
  ('20251228172549');
