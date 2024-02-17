-- migrate:up
CREATE TABLE assets (
    id INTEGER PRIMARY KEY,
    title TEXT NOT NULL UNIQUE
);

CREATE TABLE asset_entries (
    id INTEGER PRIMARY KEY,
    no TEXT NOT NULL,
    uri TEXT NOT NULL,
    was_seen INTEGER NOT NULL DEFAULT 0,
    date_uploaded TEXT NOT NULL,
    asset_id INTEGER NOT NULL,

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

-- migrate:down
DROP TABLE asset_entries;
DROP TABLE asset;

