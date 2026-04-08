-- migrate:up
CREATE TABLE authors (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);

CREATE TABLE assets_authors (
    asset_id INTEGER NOT NULL,
    author_id INTEGER NOT NULL,

    PRIMARY KEY (asset_id, author_id),

    FOREIGN KEY (asset_id) REFERENCES assets (id) ON DELETE CASCADE,
    FOREIGN KEY (author_id) REFERENCES authors (id) ON DELETE CASCADE
);

CREATE TABLE author_scraping_configs (
    id INTEGER PRIMARY KEY,
    uri TEXT NOT NULL UNIQUE,
    site TEXT NOT NULL,
    is_enabled INTEGER NOT NULL DEFAULT 1,
    author_id INTEGER NOT NULL,

    FOREIGN KEY (author_id) REFERENCES authors (id) ON DELETE CASCADE
);


-- migrate:down
DROP TABLE author_scraping_configs;
DROP TABLE assets_authors;
DROP TABLE authors;
