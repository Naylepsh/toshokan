-- migrate:up
ALTER TABLE asset_entries
ADD COLUMN title TEXT NOT NULL DEFAULT '';

UPDATE asset_entries
SET title = no;

-- migrate:down
ALTER TABLE asset_entries
DROP COLUMN title;
