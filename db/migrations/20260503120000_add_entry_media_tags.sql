-- migrate:up
CREATE TABLE tags (
  id INTEGER PRIMARY KEY,
  namespace TEXT NOT NULL,
  name TEXT NOT NULL,
  UNIQUE (namespace, name)
);

CREATE TABLE entry_media_tags (
  entry_media_id INTEGER NOT NULL,
  tag_id INTEGER NOT NULL,
  PRIMARY KEY (entry_media_id, tag_id),
  FOREIGN KEY (entry_media_id) REFERENCES entry_media (id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags (id) ON DELETE CASCADE
);

-- migrate:down
DROP TABLE entry_media_tags;
DROP TABLE tags;
