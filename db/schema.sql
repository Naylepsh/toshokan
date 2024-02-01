CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE assets (
    id INTEGER PRIMARY KEY,
    title TEXT NOT NULL UNIQUE
);
CREATE TABLE asset_entries (
    id INTEGER PRIMARY KEY,
    no TEXT NOT NULL,
    uri TEXT NOT NULL,
    asset_id INTEGER NOT NULL,

    CONSTRAINT fk_asset_id
        FOREIGN KEY (asset_id)
        REFERENCES assets (id)
        ON DELETE CASCADE
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20240130211545');
