-- migrate:up
CREATE TABLE categories (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);

INSERT INTO categories(name) VALUES ('manga'), ('manga (physical)');

ALTER TABLE assets
    ADD COLUMN category_id INTEGER
    REFERENCES categories (id);

UPDATE assets
SET category_id = categories.id
FROM categories
WHERE assets.title NOT LIKE '%physical%'
    AND categories.name = 'manga';

UPDATE assets
SET category_id = categories.id
FROM categories
WHERE assets.title LIKE '%physical%'
    AND categories.name = 'manga (physical)';

-- migrate:down
DROP TABLE categories;
