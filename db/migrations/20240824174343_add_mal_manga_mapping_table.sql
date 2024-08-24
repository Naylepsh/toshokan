-- migrate:up
CREATE TABLE mal_manga_mapping (
    id INTEGER PRIMARY KEY,
    manga_id INTEGER NOT NULL UNIQUE,
    mal_id INTEGER NOT NULL UNIQUE,

    CONSTRAINT fk_manga_id
        FOREIGN KEY (manga_id)
        REFERENCES assets (id)
        ON DELETE CASCADE
);


-- migrate:down
DROP TABLE mal_manga_mapping;
