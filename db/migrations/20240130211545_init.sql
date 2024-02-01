-- migrate:up
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

-- migrate:down
DROP TABLE asset_entries;
DROP TABLE asset;

