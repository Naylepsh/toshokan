# Entry media tagging

Tags on entry media (the folders / archives tracked by the `entryMedia` module). Eventually part of a
combined search over title, author, asset and tags; only tags and tag filtering are in scope here.
Title / author / asset search depends on the matching feature.

## Model
- A tag is a `(namespace, name)` pair. Both parts are trimmed, lowercased and have inner whitespace collapsed
  (`Rough  Translation` == `rough translation`).
- Default namespace is `tag`; a bare `drama` means `tag:drama`. Other namespaces (`language`, `parody`, `circle`, ...)
  come into existence by being used; there is no list of allowed namespaces.
- Tables: `tags(id, namespace, name, UNIQUE(namespace, name))` and
  `entry_media_tags(entry_media_id, tag_id)` (composite PK, both FKs `ON DELETE CASCADE`).
- `tags` is generic: assets could get tags later through another join table.
- A tag that no media uses any more is removed.

## Entering tags
Comma-separated field: `language:english, drama, rough translation`. Commas separate tags, so multi-word
names need no quotes.

## Search syntax
Whitespace-separated tokens:
- `namespace:value` -> tag filter, all of them combined with AND. Multi-word values are quoted: `tag:"rough translation"`.
- Everything else is free text: each word must appear (case-insensitive substring) in the media name, so
  `frieren one` matches `Frieren One`. A bare word is text, never a tag.
- Example: `frieren language:english tag:drama`.
- Values match exactly (after normalization).
- Keys `title:`, `author:`, `asset:` are not reserved in code yet; until the matching feature lands they behave as
  ordinary namespaces.

## Out of scope for v1, wanted later
- Negation: `-tag:drama` (exclude).
- OR: `language:english|japanese`, or `tag:drama OR tag:comedy`.
- Prefix / wildcard matching on tag values: `tag:rough*`.
- Renaming and merging tags (e.g. `eng` -> `english`).
- Tag autocomplete when entering tags and in the search bar.
- Tags on assets / entries.
- `title:`, `author:`, `asset:` search keys (needs the matching feature).
- Tag counts / a tag overview page.
