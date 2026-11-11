# Run from the directory containing outputs/ and data/youtube/.
# No API calls. The submitted page does not need this script to render.
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
dir.create("outputs", showWarnings = FALSE)
comments_all <- readRDS("data/youtube/comments_all.rds")
stopifnot(!anyDuplicated(comments_all$comment_id),
          !anyNA(comments_all$channel_title))
# 1. 채널·영상 게시 시점별 댓글 수와 답글 비율
collection_summary <- comments_all |>
  group_by(channel_title, period) |>
  summarise(
    comments = n(),
    replies = sum(is_reply),
    reply_share = mean(is_reply),
    .groups = "drop"
  )

collection_summary

# 2. 좋아요 분포와 상위 1% 댓글의 좋아요 점유율
likes <- replace_na(comments_all$like_count, 0)
top_n <- ceiling(length(likes) * 0.01)

like_summary <- tibble(
  total_likes = sum(likes),
  median_likes = median(likes),
  p90_likes = unname(quantile(likes, 0.90)),
  max_likes = max(likes),
  top_1pct_comments = top_n,
  top_1pct_like_share = if (sum(likes) > 0) {
    sum(sort(likes, decreasing = TRUE)[seq_len(top_n)]) / sum(likes)
  } else {
    NA_real_
  }
)

like_summary

# 3. 작성자 집중도: ID가 확인되는 작성자 기준
author_counts <- comments_all |>
  filter(!is.na(author_channel_id), author_channel_id != "") |>
  count(author_channel_id, sort = TRUE)

top_authors_n <- ceiling(nrow(author_counts) * 0.10)

author_summary <- tibble(
  identified_authors = nrow(author_counts),
  comments_with_author_id = sum(author_counts$n),
  comments_without_author_id = sum(
    is.na(comments_all$author_channel_id) |
      comments_all$author_channel_id == ""
  ),
  top_10pct_author_share = if (nrow(author_counts) > 0) {
    sum(head(author_counts$n, top_authors_n)) / sum(author_counts$n)
  } else {
    NA_real_
  }
)

author_summary

# 4. 정확히 동일한 댓글 텍스트: 공백·대소문자를 변경하지 않고 비교
text_counts <- comments_all |>
  filter(!is.na(text_original), text_original != "") |>
  count(text_original)

duplicate_summary <- text_counts |>
  summarise(
    repeated_text_groups = sum(n > 1),
    comments_in_repeated_groups = sum(n[n > 1]),
    extra_duplicate_copies = sum(pmax(n - 1L, 0L))
  )

duplicate_summary


# 분석용 텍스트 준비
comments_all <- comments_all |>
  mutate(
    text_analysis = coalesce(text_original, text_display, ""),
    text_analysis = str_remove_all(
      text_analysis,
      regex("https?://\\S+|www\\.\\S+", ignore_case = TRUE)
    ),
    text_analysis = str_remove_all(
      text_analysis,
      "(?<!\\S)@\\S+"
    ),
    text_analysis = str_squish(text_analysis)
  )

# 작성자 정보는 코퍼스에 넣지 않음
corpus_data <- comments_all |>
  select(
    comment_id, text_analysis,
    channel_title, video_id, is_reply, like_count,
    date, period, comment_period
  )

yt_corpus <- corpus(
  corpus_data,
  text_field = "text_analysis",
  docid_field = "comment_id"
)

# 문맥 확인용 토큰: 불용어는 아직 제거하지 않음
yt_tokens <- tokens(
  yt_corpus,
  remove_punct = TRUE,
  remove_symbols = FALSE
) |>
  tokens_tolower()

# 필수 검색어별 문맥 최대 20개
kwic_patterns <- c(
  "democrat*", "republican*", "trump", "vote*", "ballot*"
)

kwic_review <- set_names(kwic_patterns) |>
  map(\(word) {
    kwic(
      yt_tokens,
      pattern = word,
      valuetype = "glob",
      window = 7
    ) |>
      as.data.frame() |>
      slice_head(n = 20)
  })

# 먼저 democrat*의 문맥 확인
print(kwic_review[["democrat*"]], row.names = FALSE)


for (word in c("republican*", "trump", "vote*", "ballot*")) {
  cat("\n검색어:", word, "\n")
  
  kwic_review[[word]] |>
    as_tibble() |>
    mutate(line = row_number()) |>
    select(line, pre, keyword, post) |>
    print(n = 20, width = Inf)
}


# 필수 구문 5개 + 문맥에서 선정한 구문 2개
phrase_list <- c(
  "house majority",
  "senate race",
  "early voting",
  "mail in",
  "election fraud",
  "vote blue",
  "voter fraud"
)

# 여러 단어를 하나의 특성으로 결합
yt_tokens_compound <- tokens_compound(
  yt_tokens,
  pattern = phrase(phrase_list),
  concatenator = "_"
)

# 구문 결합 후 불용어 제거
yt_tokens_features <- tokens_remove(
  yt_tokens_compound,
  pattern = stopwords("en")
)

yt_dfm <- dfm(yt_tokens_features)

# 구문별 등장 횟수 확인: 없으면 0
phrase_features <- str_replace_all(phrase_list, " ", "_")
present <- phrase_features %in% featnames(yt_dfm)

phrase_counts <- tibble(
  phrase = phrase_list,
  frequency = 0
)

phrase_counts$frequency[present] <- as.numeric(
  colSums(yt_dfm[, phrase_features[present], drop = FALSE])
)

phrase_counts

# 전체 상위 특성 20개
topfeatures(yt_dfm, 20)

# 문서 수 확인
ndoc(yt_dfm)



# 각 채널 vs 나머지 두 채널
channel_keyness <- set_names(
  c("Al Jazeera English", "CNN", "NBC News")
) |>
  map(\(channel) {
    textstat_keyness(
      yt_dfm,
      target = docvars(yt_dfm, "channel_title") == channel,
      measure = "chi2"
    )
  })

# 각 채널에서 상대적으로 두드러지는 특성 10개
for (channel in names(channel_keyness)) {
  cat("\n채널:", channel, "\n")
  
  channel_keyness[[channel]] |>
    as_tibble() |>
    filter(chi2 > 0) |>
    slice_head(n = 10) |>
    select(feature, chi2, n_target, n_reference) |>
    print(n = 10, width = Inf)
}

# 선거 후 게시 영상 vs 선거 전 게시 영상
period_keyness <- textstat_keyness(
  yt_dfm,
  target = docvars(yt_dfm, "period") == "after",
  measure = "chi2"
)

cat("\n선거 후 게시 영상에서 두드러지는 특성\n")
period_keyness |>
  as_tibble() |>
  filter(chi2 > 0) |>
  slice_head(n = 10) |>
  select(feature, chi2, n_target, n_reference) |>
  print(n = 10, width = Inf)

cat("\n선거 전 게시 영상에서 두드러지는 특성\n")
period_keyness |>
  as_tibble() |>
  filter(chi2 < 0) |>
  arrange(chi2) |>
  slice_head(n = 10) |>
  select(feature, chi2, n_target, n_reference) |>
  print(n = 10, width = Inf)





# CNN keyness 상위 10개 중 아랍 문자로 된 특성
arabic_features <- channel_keyness[["CNN"]] |>
  as_tibble() |>
  slice_head(n = 10) |>
  filter(str_detect(feature, "\\p{script=Arabic}")) |>
  pull(feature)

# 해당 특성이 각 댓글에서 몇 번 등장하는지
arabic_hits <- as.numeric(
  rowSums(yt_dfm[, arabic_features, drop = FALSE])
)

arabic_check <- tibble(
  channel_title = docvars(yt_dfm, "channel_title"),
  period = docvars(yt_dfm, "period"),
  hits = arabic_hits
)

arabic_summary <- arabic_check |>
  group_by(channel_title, period) |>
  summarise(
    comments_with_features = sum(hits > 0),
    feature_occurrences = sum(hits),
    max_in_one_comment = max(hits),
    .groups = "drop"
  )

arabic_summary

# 전체 출현 횟수 중 가장 많이 반복한 댓글 5개의 비중
tibble(
  top5_comment_share = if (sum(arabic_hits) > 0) {
    sum(head(sort(arabic_hits, decreasing = TRUE), 5)) /
      sum(arabic_hits)
  } else {
    NA_real_
  }
)


# 해당 특성이 나타난 3개 댓글만 제외한 비교용 DFM
yt_dfm_sensitivity <- yt_dfm[arabic_hits == 0, ]

# CNN vs 나머지 두 채널 재분석
cnn_keyness_sensitivity <- textstat_keyness(
  yt_dfm_sensitivity,
  target = docvars(
    yt_dfm_sensitivity, "channel_title"
  ) == "CNN",
  measure = "chi2"
)

cnn_keyness_sensitivity |>
  as_tibble() |>
  filter(chi2 > 0) |>
  slice_head(n = 10) |>
  select(feature, chi2, n_target, n_reference) |>
  print(n = 10, width = Inf)

# 영상 게시 시점별 비교도 재분석
period_keyness_sensitivity <- textstat_keyness(
  yt_dfm_sensitivity,
  target = docvars(yt_dfm_sensitivity, "period") == "after",
  measure = "chi2"
)

# 선거 전 게시 영상에서 두드러지는 특성
period_keyness_sensitivity |>
  as_tibble() |>
  filter(chi2 < 0) |>
  arrange(chi2) |>
  slice_head(n = 10) |>
  select(feature, chi2, n_target, n_reference) |>
  print(n = 10, width = Inf)




# LSD2015 사전의 긍정·부정 항목 사용
sentiment_dictionary <- data_dictionary_LSD2015[
  c("positive", "negative")
]

sentiment_dfm <- dfm(yt_tokens) |>
  dfm_lookup(dictionary = sentiment_dictionary)

sentiment_counts <- as.data.frame(
  as.matrix(sentiment_dfm)
) |>
  as_tibble()

sentiment_results <- tibble(
  comment_id = docnames(sentiment_dfm),
  channel_title = docvars(sentiment_dfm, "channel_title"),
  period = docvars(sentiment_dfm, "period")
) |>
  bind_cols(sentiment_counts) |>
  mutate(
    score = positive - negative,
    dictionary_label = case_when(
      score > 0 ~ "positive",
      score < 0 ~ "negative",
      TRUE ~ "neither"
    ),
    matched_words = positive + negative
  )

sentiment_summary <- sentiment_results |>
  group_by(channel_title, period) |>
  summarise(
    comments = n(),
    positive_share = mean(dictionary_label == "positive"),
    negative_share = mean(dictionary_label == "negative"),
    neither_share = mean(dictionary_label == "neither"),
    no_match_share = mean(matched_words == 0),
    .groups = "drop"
  )

print(sentiment_summary, width = Inf)

saveRDS(
  sentiment_results,
  "data/youtube/sentiment_results.rds"
)

# Preserve existing manual labels only for their exact sampled comment IDs.
# New raw data require a new 40-comment sample and fresh manual coding.
set.seed(6302)
new_sample <- comments_all |>
  arrange(comment_id) |> slice_sample(n = 40) |>
  transmute(sample_no = row_number(), comment_id,
            text = coalesce(text_original, text_display, ""),
            human_label = NA_character_)
coding_file <- "data/youtube/validation_40.csv"
if (file.exists(coding_file)) {
  old_coding <- read_csv(coding_file, show_col_types = FALSE)
  stopifnot(!anyDuplicated(old_coding$comment_id))
  validation_sample <- new_sample |> select(-human_label) |>
    left_join(old_coding |> select(comment_id, human_label),
              by = "comment_id", relationship = "many-to-one")
} else {
  validation_sample <- new_sample
}
if (anyNA(validation_sample$human_label)) {
  write_csv(validation_sample, "data/youtube/validation_40_to_code.csv")
  stop("Code the new sample, save as validation_40.csv, and rerun analysis.")
}
# 40개 모두 분류됐는지 확인
stopifnot(
  !anyNA(validation_sample$human_label),
  all(validation_sample$human_label %in%
        c("positive", "negative", "neither"))
)

# 사전 결과 연결
validation_results <- validation_sample |>
  left_join(
    sentiment_results |>
      select(comment_id, dictionary_label),
    by = "comment_id",
    relationship = "many-to-one"
  ) |>
  mutate(agreement = human_label == dictionary_label)

# 일치 개수와 일치율
validation_results |>
  summarise(
    n = n(),
    agreed = sum(agreement),
    agreement_rate = mean(agreement)
  )

# 사람이 붙인 분류와 사전 분류 비교
with(
  validation_results,
  table(
    Human = human_label,
    Dictionary = dictionary_label
  )
)

# 불일치 사례 번호만 확인
validation_results |>
  filter(!agreement) |>
  select(sample_no, human_label, dictionary_label) |>
  print(n = 40)

saveRDS(
  validation_results,
  "data/youtube/validation_results.rds"
)



dir.create("outputs", showWarnings = FALSE)

write_csv(collection_summary, "outputs/collection_summary.csv")
write_csv(like_summary, "outputs/like_summary.csv")
write_csv(author_summary, "outputs/author_summary.csv")
write_csv(duplicate_summary, "outputs/duplicate_summary.csv")
write_csv(phrase_counts, "outputs/phrase_counts.csv")
write_csv(sentiment_summary, "outputs/sentiment_summary.csv")

validation_summary <- validation_results |>
  summarise(
    n = n(),
    agreed = sum(agreement),
    agreement_rate = mean(agreement),
    majority_class_baseline = max(table(human_label)) / n()
  )

validation_confusion <- validation_results |>
  count(human_label, dictionary_label, name = "n")

write_csv(
  validation_summary,
  "outputs/validation_summary.csv"
)
write_csv(
  validation_confusion,
  "outputs/validation_confusion.csv"
)


# 그래프에 표시할 이름과 순서
period_labels <- c(
  before = "Before-election videos",
  after = "After-election videos"
)

# 1. 채널·영상 게시 시점별 수집 댓글 수
p_collection <- collection_summary |>
  mutate(period = factor(period, levels = c("before", "after"))) |>
  ggplot(aes(
    x = channel_title, y = comments, fill = period
  )) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = comments),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(before = "#4477AA", after = "#EEAA33"),
    labels = period_labels
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Comments collected by channel and video period",
    subtitle = "Three videos per channel per period; up to 300 comments per video",
    x = NULL, y = "Comments collected", fill = "Video publication period"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_collection

# 2. 사전 감성 분류 비율
sentiment_plot_data <- sentiment_summary |>
  select(
    channel_title, period,
    positive_share, negative_share, neither_share
  ) |>
  pivot_longer(
    ends_with("_share"),
    names_to = "sentiment",
    values_to = "share"
  ) |>
  mutate(
    period = factor(period, levels = c("before", "after")),
    sentiment = factor(
      sentiment,
      levels = c("positive_share", "neither_share", "negative_share"),
      labels = c("Positive", "Neither / tied", "Negative")
    )
  )

p_sentiment <- ggplot(
  sentiment_plot_data,
  aes(x = channel_title, y = share, fill = sentiment)
) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
  facet_wrap(
    ~ period,
    labeller = labeller(period = period_labels)
  ) +
  scale_fill_manual(
    values = c(
      "Positive" = "#228833",
      "Neither / tied" = "#BBBBBB",
      "Negative" = "#CC6677"
    )
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Dictionary sentiment by channel and video period",
    subtitle = "LSD2015 positive and negative word counts",
    caption = paste(
      "Agreement with AI-assisted human coding: 57.5% (40 comments).",
      "\nNeither / tied includes comments with no dictionary matches."
    ),
    x = NULL, y = "Share of comments", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

p_sentiment

ggsave(
  "outputs/comments_by_channel.png",
  p_collection,
  width = 9, height = 5, dpi = 300
)

ggsave(
  "outputs/sentiment_by_channel.png",
  p_sentiment,
  width = 10, height = 6, dpi = 300
)

imap_dfr(channel_keyness, \(x, channel)
  as_tibble(x) |> mutate(channel = channel, .before = 1)) |>
  write_csv("outputs/keyness_channels.csv")
write_csv(as_tibble(period_keyness), "outputs/keyness_video_period.csv")
write_csv(as_tibble(cnn_keyness_sensitivity),
          "outputs/keyness_cnn_sensitivity.csv")
write_csv(as_tibble(period_keyness_sensitivity),
          "outputs/keyness_period_sensitivity.csv")
