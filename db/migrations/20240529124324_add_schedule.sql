-- migrate:up
CREATE TABLE scraping_schedules (
    id INTEGER PRIMARY KEY,
    category_id INTEGER NOT NULL,
    day_of_week INTEGER NOT NULL,
    min_days_since_last_scrape INTEGER NOT NULL,

    CONSTRAINT fk_category_id
        FOREIGN KEY (category_id)
        REFERENCES categories (id)
        ON DELETE CASCADE
);

-- migrate:down
DROP TABLE scraping_schedules;
