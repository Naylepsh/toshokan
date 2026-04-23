-- migrate:up
CREATE TABLE author_aliases (
  alias_name TEXT PRIMARY KEY,
  author_id INTEGER NOT NULL,
  FOREIGN KEY (author_id) REFERENCES authors (id) ON DELETE CASCADE
);

-- migrate:down
DROP TABLE author_aliases;
