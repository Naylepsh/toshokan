-- migrate:up
CREATE TABLE assets (
    id INTEGER PRIMARY KEY,
    title TEXT
);

-- migrate:down
DROP TABLE asset;

