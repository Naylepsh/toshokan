-- migrate:up
CREATE TABLE author_scraping_schedules (
    id INTEGER PRIMARY KEY,
    day_of_week INTEGER NOT NULL
);

-- Seed with Saturday and Sunday (matching old doujinshi schedule)
INSERT INTO author_scraping_schedules (day_of_week) VALUES (6), (7);

-- migrate:down
DROP TABLE author_scraping_schedules;
