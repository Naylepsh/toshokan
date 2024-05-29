-- migrate:up

ALTER TABLE scraping_schedules 
DROP COLUMN min_days_since_last_scrape;

-- migrate:down
ALTER TABLE scraping_schedules 
ADD COLUMN min_days_since_last_scrape INTEGER NOT NULL DEFAULT 0;
