if (Sys.getenv("RUN_YOUTUBE_COLLECTION") != "yes")
  stop("Set RUN_YOUTUBE_COLLECTION=yes only when ready to make API calls.")

library(tuber)
library(tidyverse)

stopifnot(nzchar(Sys.getenv("YOUTUBE_KEY")))
dir.create("data/youtube", recursive = TRUE, showWarnings = FALSE)
ignored <- if (file.exists(".gitignore")) readLines(".gitignore") else character()
writeLines(unique(c(ignored, "data/", ".Renviron")), ".gitignore")
manifests <- set_names(c("before", "after")) |>
  map(\(p) jsonlite::read_json(
    paste0("outputs/manifest_", p, ".json"), simplifyVector = TRUE))


rerun_plan <- tibble(
  stage = c("searches", "channel details", "uploads", "video stats", "comments"),
  search_calls = c(8L, 0L, 0L, 0L, 0L),
  data_units_estimate = c(0L, 1L, 360L, 2L, 54L))

write_csv(rerun_plan, "data/youtube/rerun_quota_plan.csv")
print(rerun_plan)

channels <- as_tibble(manifests$before$channels)
channel_details <- get_channel_details(channels$channel_id)
channel_uploads <- set_names(channels$channel_id, channels$channel_title) |>
  map(\(id) list_channel_videos(id, max_results = 6000) |>
        as_tibble() |> mutate(date = as.Date(video_published_at)))
saveRDS(channel_uploads, "data/youtube/channel_uploads.rds")
saveRDS(channel_details, "data/youtube/channel_details.rds")
coverage <- imap_dfr(channel_uploads, \(x, channel) tibble(
  channel, earliest = min(x$date, na.rm = TRUE)))

write_csv(coverage, "data/youtube/rerun_upload_coverage.csv")

if (any(coverage$earliest >= as.Date("2026-09-01")))
  warning("Uploads no longer cover September 1; plan more pages if needed.")
collect_comments <- function(video_id, n_max = 300) {
  status <- "ok"
  
  out <- tryCatch(
    withCallingHandlers(
      get_all_comments(video_id, max_results = n_max),
      tuber_comments_disabled = function(w) {
        status <<- "disabled"
        invokeRestart("muffleWarning")
      },
      tuber_no_comments = function(w) {
        if (status == "ok") status <<- "none"
        invokeRestart("muffleWarning")}),
    error = function(e) e)
  
  Sys.sleep(1)
  
  if (inherits(out, "error")) {
    return(list(
      data = tibble(),
      log = tibble(
        video_id = video_id, status = "error", n = 0L,
        message = conditionMessage(out))))}
  
  list(
    data = as_tibble(out),
    log = tibble(
      video_id = video_id, status = status,
      n = nrow(out), message = NA_character_))}

all_comments <- list()
all_logs <- list()

for (period_name in c("before", "after")) {
  m <- manifests[[period_name]]
  search_results <- set_names(m$search_orders) |>
    map(\(ordering) yt_search(
      term = m$search_term, published_after = m$published_after,
      published_before = m$published_before,
      order = ordering, max_results = 100))
  saveRDS(search_results, paste0("data/youtube/search_", period_name, ".rds"))
  targets <- as_tibble(m$selected_videos) |>
    select(-comment_count) |> mutate(date = as.Date(date))
  stats <- get_video_stats(targets$video_id)
  targets <- targets |> left_join(
    stats |> select(video_id, view_count, comment_count),
    by = "video_id", relationship = "many-to-one")
  runs <- map(targets$video_id, \(id) {
    z <- collect_comments(id, n_max = 300)
    saveRDS(z, paste0("data/youtube/", period_name, "_", id, ".rds"))
    z})

  comments <- map2_dfr(runs, targets$video_id,
                      \(z, id) z$data |> mutate(video_id = id)) |>
    distinct(comment_id, .keep_all = TRUE) |>
    left_join(targets |> select(video_id, channel_title,
                               video_title = title, video_date = date),
              by = "video_id", relationship = "many-to-one")
  log <- map_dfr(runs, "log") |>
    mutate(collected_at = format(Sys.time(), tz = "UTC", usetz = TRUE)) |>
    left_join(targets |> select(video_id, channel_title, comment_count),
              by = "video_id", relationship = "many-to-one")
  saveRDS(comments, paste0("data/youtube/comments_", period_name, ".rds"))
  saveRDS(targets, paste0("data/youtube/targets_", period_name, ".rds"))
  write_csv(log, paste0("data/youtube/log_", period_name, ".csv"))
  m$collected_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  m$delete_by <- as.character(as.Date(Sys.time(), tz = "UTC") + 30)
  m$tuber_version <- as.character(packageVersion("tuber"))
  m$selected_videos <- targets
  m$selection <- "Re-request of exact selected video IDs in original manifest."
  m$timing_note <- "New retrospective collection; not the historical snapshot."
  jsonlite::write_json(m, paste0("data/youtube/manifest_", period_name, ".json"),
                       pretty = TRUE, auto_unbox = TRUE, dataframe = "rows")
  all_comments[[period_name]] <- comments |> mutate(period = period_name)
  all_logs[[period_name]] <- log |> mutate(period = period_name)}

comments_all <- bind_rows(all_comments) |>
  distinct(comment_id, .keep_all = TRUE) |>
  mutate(is_reply = !is.na(parent_id) & parent_id != "",
         date = as.Date(substr(published_at, 1, 10)),
         comment_period = case_when(
           date < as.Date("2026-11-03") ~ "before",
           date == as.Date("2026-11-03") ~ "election_day",
           date > as.Date("2026-11-03") ~ "after",
           TRUE ~ NA_character_))

saveRDS(comments_all, "data/youtube/comments_all.rds")

write_csv(bind_rows(all_logs), "data/youtube/rerun_collection_log.csv")
write_csv(yt_get_quota_usage(), "data/youtube/rerun_quota_usage.csv")
