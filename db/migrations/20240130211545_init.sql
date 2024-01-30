-- migrate:up
CREATE TABLE asset (
    id INTEGER PRIMARY KEY,
    Title TEXT
);

-- migrate:down
DROP TABLE asset;

