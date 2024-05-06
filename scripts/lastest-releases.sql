select substr(title, 0, 64), asset_scraping_configs.uri, latest_release
from assets
join (
  select asset_id, max(date_uploaded) as latest_release
  from asset_entries
  group by asset_id
) as latest_entries on latest_entries.asset_id = assets.id
join asset_scraping_configs on asset_scraping_configs.asset_id = assets.id
where is_enabled = 1
order by latest_release asc
