-- migrate:up
CREATE TABLE entry_progress (
    entry_id INTEGER PRIMARY KEY,
    was_seen BOOLEAN NOT NULL DEFAULT FALSE,
    date_marked_seen DATETIME,
    FOREIGN KEY (entry_id) REFERENCES asset_entries(id) ON DELETE CASCADE
);

-- Migrate existing data from asset_entries.was_seen to entry_progress
INSERT INTO entry_progress (entry_id, was_seen, date_marked_seen)
SELECT id, was_seen, CASE WHEN was_seen = 1 THEN datetime('now') ELSE NULL END
FROM asset_entries
WHERE was_seen = 1;

-- Remove was_seen column from asset_entries
ALTER TABLE asset_entries DROP COLUMN was_seen;

-- migrate:down
-- Add was_seen column back to asset_entries
ALTER TABLE asset_entries ADD COLUMN was_seen BOOLEAN NOT NULL DEFAULT FALSE;

-- Migrate data back from entry_progress to asset_entries
UPDATE asset_entries 
SET was_seen = (
    SELECT COALESCE(ep.was_seen, FALSE)
    FROM entry_progress ep 
    WHERE ep.entry_id = asset_entries.id
);

DROP TABLE entry_progress;
