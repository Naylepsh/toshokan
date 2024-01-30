CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE asset (
    id INTEGER PRIMARY KEY,
    Title TEXT
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20240130211545');
