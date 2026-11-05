# 経済センサス2009/2021 1kmメッシュ従業者数の比較マップ（モデルゾーン key_code_sf 上）
# 出力: project_log/scenario-analysis/plots/employment_census_2009_2021/

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

load("data/key_code_sf.xdr") # key_code_sf

out_dir <- "project_log/scenario-analysis/plots/employment_census_2009_2021"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_emp_csv <- function(path, col) {
  read.csv(path, fileEncoding = "CP932", stringsAsFactors = FALSE, na.strings = c("", "NA")) %>%
    filter(!is.na(KEY_CODE)) %>%
    transmute(KEY_CODE = as.character(KEY_CODE),
              emp = replace_na(suppressWarnings(as.numeric(.data[[col]])), 0))
}

emp2009_raw <- read_emp_csv("data/raw/経済センサス2009/tblT000616S5030.csv", "T000616002")   # 全産業従業者数
emp2021_raw <- read_emp_csv("data/raw/経済センサス2021/tblT001146S5030.csv", "T001146022")   # Ａ～Ｓ全産業従業者数

emp_sf <- key_code_sf %>%
  left_join(emp2009_raw %>% rename(emp2009 = emp), by = "KEY_CODE") %>%
  left_join(emp2021_raw %>% rename(emp2021 = emp), by = "KEY_CODE") %>%
  mutate(across(c(emp2009, emp2021), ~ replace_na(.x, 0)),
         emp_diff = emp2021 - emp2009)

sym_limits <- function(data, vars) {
  lim <- max(abs(unlist(sf::st_drop_geometry(data)[vars])), na.rm = TRUE)
  c(-lim, lim)
}

map_theme <- theme_void() +
  theme(plot.title = element_text(size = 11, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA))

level_lim <- c(0, max(emp_sf$emp2009, emp_sf$emp2021))
make_level_map <- function(var, title) {
  ggplot(emp_sf) +
    geom_sf(aes(fill = .data[[var]]), color = "gray50", linewidth = 0.1) +
    scale_fill_viridis_c(option = "magma", direction = -1, limits = level_lim,
                         trans = scales::pseudo_log_trans(base = 10),
                         breaks = c(0, 100, 1000, 10000, 100000),
                         labels = scales::label_comma(), name = "従業者数") +
    labs(title = title) +
    map_theme
}

p_2009 <- make_level_map("emp2009", "従業者数 2009年（経済センサス）")
p_2021 <- make_level_map("emp2021", "従業者数 2021年（経済センサス）")

p_diff <- ggplot(emp_sf) +
  geom_sf(aes(fill = emp_diff), color = "gray70", linewidth = 0.1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = sym_limits(emp_sf, "emp_diff"),
                       labels = scales::label_comma(), name = "増減（人）") +
  labs(title = "従業者数の増減（2021年 − 2009年）") +
  map_theme

# 都心の大きな増減に引っ張られて郊外が白くなるため、疑似対数スケール版も作る
p_diff_log <- p_diff +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = sym_limits(emp_sf, "emp_diff"),
                       trans = scales::pseudo_log_trans(sigma = 10, base = 10),
                       breaks = c(-10000, -1000, -100, 0, 100, 1000, 10000),
                       labels = scales::label_comma(), name = "増減（人）") +
  labs(title = "従業者数の増減（2021年 − 2009年、疑似対数スケール）")

p_compare <- p_2009 + p_2021 + p_diff

print(p_2009)
ggsave(file.path(out_dir, "emp2009_map.png"), p_2009, width = 7, height = 6, dpi = 150)
print(p_2021)
ggsave(file.path(out_dir, "emp2021_map.png"), p_2021, width = 7, height = 6, dpi = 150)
print(p_diff)
ggsave(file.path(out_dir, "emp_diff_2021_2009_map.png"), p_diff, width = 7, height = 6, dpi = 150)
print(p_diff_log)
ggsave(file.path(out_dir, "emp_diff_2021_2009_map_pseudolog.png"), p_diff_log, width = 7, height = 6, dpi = 150)
print(p_compare)
ggsave(file.path(out_dir, "emp_2009_2021_compare.png"), p_compare, width = 18, height = 6, dpi = 150)

# 集計値を summary.md に書き出す（実行ごとに上書き）
emp_df <- st_drop_geometry(emp_sf)
fmt <- function(x) format(round(x), big.mark = ",")
top_tbl <- function(d) {
  c("| KEY_CODE | 2009 | 2021 | 増減 |", "|---|---:|---:|---:|",
    sprintf("| %s | %s | %s | %s |", d$KEY_CODE, fmt(d$emp2009), fmt(d$emp2021), fmt(d$emp_diff)))
}
tot09 <- sum(emp2009_raw$emp); tot21 <- sum(emp2021_raw$emp)
in09 <- sum(emp_df$emp2009);   in21 <- sum(emp_df$emp2021)

summary_lines <- c(
  "# 経済センサス2009/2021 従業者数 集計（自動生成）",
  "",
  paste0("- 生成: ", format(Sys.time(), "%Y-%m-%d %H:%M"), " / `scripts/employment_census_2009_2021.R`"),
  paste0("- 対象ゾーン: `key_code_sf` ", nrow(emp_df), " メッシュ"),
  "",
  "| | 2009 | 2021 | 増減 |", "|---|---:|---:|---:|",
  sprintf("| CSV全体の合計 | %s | %s | %s |", fmt(tot09), fmt(tot21), fmt(tot21 - tot09)),
  sprintf("| モデルゾーン内の合計 | %s | %s | %s |", fmt(in09), fmt(in21), fmt(in21 - in09)),
  sprintf("| ゾーン内カバー率 | %.1f%% | %.1f%% | |", 100 * in09 / tot09, 100 * in21 / tot21),
  "",
  sprintf("- 増加ゾーン: %d / 減少ゾーン: %d / 変化なし: %d",
          sum(emp_df$emp_diff > 0), sum(emp_df$emp_diff < 0), sum(emp_df$emp_diff == 0)),
  "",
  "## 増加上位10ゾーン", "",
  top_tbl(emp_df %>% arrange(desc(emp_diff)) %>% head(10)),
  "",
  "## 減少上位10ゾーン", "",
  top_tbl(emp_df %>% arrange(emp_diff) %>% head(10))
)
writeLines(summary_lines, file.path(out_dir, "summary.md"))
