-- migrate:up
ALTER TABLE external_chapters RENAME TO entry_media;

-- migrate:down
ALTER TABLE entry_media RENAME TO external_chapters;
