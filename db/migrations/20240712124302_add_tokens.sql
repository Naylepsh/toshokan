-- migrate:up
CREATE TABLE tokens (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    value TEXT NOT NULL UNIQUE,
    expires_at INTEGER NOT NULL
);

-- migrate:down
DROP TABLE tokens;

