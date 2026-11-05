# 経済センサス2009/2021の雇用分布によるシナリオ分析
# 総従業者数は現状scn0の総数に、総給与(Σω_j L_j)もscn0に揃える。方針は project_log/scenario-analysis/README.md
# 同じセッションでメインスクリプトを通した後なら追加分だけ実行される

# メイン末尾の論文用プロット節は未定義オブジェクトで止まるため、エラーでも必要なオブジェクトが揃っていれば続行
if (!exists("run_scenario")) {
  tryCatch(source("scripts/Fukuoka_LUTI_model_260617.R"),
           error = function(e) message("メインスクリプトがエラーで停止: ", conditionMessage(e)))
}
needed <- c("run_scenario", "results", "scn0_L_j_hat", "voronoi_points", "dist_rail.km",
            "build_scn_rail_diff_map_data", "make_diff_map2")
if (!all(sapply(needed, exists))) stop("メインスクリプトの必要なオブジェクトが不足: ",
                                       paste(needed[!sapply(needed, exists)], collapse = ", "))

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

emp_run_dir  <- file.path("project_log", "scenario-analysis", "runs", active_calibration, "employment_2009_2021")
emp_plot_dir <- file.path(emp_run_dir, "plots")
for (d in c("input", "landuse", "road", "rail")) {
  dir.create(file.path(emp_plot_dir, d), recursive = TRUE, showWarnings = FALSE)
}

#年別 L_j_hat（ボロノイ集計→scn0総数にスケーリング）####
read_emp_csv <- function(path, col) {
  read.csv(path, fileEncoding = "CP932", stringsAsFactors = FALSE, na.strings = c("", "NA")) %>%
    filter(!is.na(KEY_CODE)) %>%
    transmute(KEY_CODE = as.character(KEY_CODE),
              emp = replace_na(suppressWarnings(as.numeric(.data[[col]])), 0))
}

total_L0 <- sum(scn0_L_j_hat$L_j_hat, na.rm = TRUE)

make_L_j_hat_year <- function(emp_df) {
  agg <- voronoi_points %>%
    st_drop_geometry() %>%
    dplyr::select(KEY_CODE = KEY_CODE.x, center = KEY_CODE.y) %>%
    left_join(emp_df, by = "KEY_CODE") %>%
    mutate(emp = replace_na(emp, 0)) %>%
    group_by(center) %>%
    summarise(emp = sum(emp), .groups = "drop")
  raw <- agg$emp[match(rownames(scn0_L_j_hat), agg$center)]
  raw[is.na(raw)] <- 0
  scaled <- round(raw * total_L0 / sum(raw))
  i_max <- which.max(scaled)
  scaled[i_max] <- scaled[i_max] + (total_L0 - sum(scaled))  # 丸めによる合計のずれを最大ゾーンで吸収
  out <- scn0_L_j_hat
  out$L_j_hat <- scaled
  list(L_j_hat = out, raw_total = sum(raw), scale = total_L0 / sum(raw))
}

emp_years <- list(
  emp2009 = make_L_j_hat_year(read_emp_csv("data/raw/経済センサス2009/tblT000616S5030.csv", "T000616002")),
  emp2021 = make_L_j_hat_year(read_emp_csv("data/raw/経済センサス2021/tblT001146S5030.csv", "T001146022"))
)

#賃金（ω_j固定、総給与をscn0に揃える）####
payroll0 <- sum(omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE)
for (y in names(emp_years)) {
  k_w <- payroll0 / sum(omega_j$omega_j * emp_years[[y]]$L_j_hat$L_j_hat, na.rm = TRUE)
  emp_years[[y]]$wage_k <- k_w
  emp_years[[y]]$omega_j_matrix <- scn0_omega_j_matrix * k_w
}

#実行####
emp_msa <- 0.5  # 既存シナリオB/Cと同じ
emp_results <- list()
for (y in names(emp_years)) {
  cat("\n=== Running:", y, "===\n")
  emp_results[[y]] <- run_scenario(
    scenario_name = y,
    L_j_hat_scn = emp_years[[y]]$L_j_hat,
    omega_j_matrix_scn = emp_years[[y]]$omega_j_matrix,
    v_init = results$scn0$v,
    max_iter = 50, msa = emp_msa, tol = 100
  )
}
all_res <- c(list(scn0 = results$scn0), emp_results)
lab <- c(scn0 = "現状", emp2009 = "2009年雇用分布", emp2021 = "2021年雇用分布")

#入力（L_j_hat）マップ####
input_sf <- key_code_sf %>%
  left_join(tibble(KEY_CODE = rownames(scn0_L_j_hat),
                   L_scn0 = scn0_L_j_hat$L_j_hat,
                   L_2009 = emp_years$emp2009$L_j_hat$L_j_hat,
                   L_2021 = emp_years$emp2021$L_j_hat$L_j_hat), by = "KEY_CODE") %>%
  mutate(L_diff = L_2021 - L_2009)

p_input <- make_diff_map2(input_sf, "L_diff", "従業者数 L_j_hat（2021 − 2009、スケール後）", "(人)")
print(p_input)
ggsave(file.path(emp_plot_dir, "input/L_j_hat_diff_2021_2009.png"), p_input, width = 7, height = 6, dpi = 150)

#土地利用マップ####
zone_vals <- function(res) {
  s <- res$state
  hh <- rowSums(s$l_i_j, na.rm = TRUE)
  tibble(KEY_CODE  = gsub("^mc_", "", rownames(s$l_i_j)),
         household = hh,
         rent      = as.numeric(s$r_bar_i) * 1000,                       # 円
         floor     = rowSums(s$a_fij_H * s$l_i_j, na.rm = TRUE) / hh,    # m2/世帯
         Gi        = as.numeric(s$G_i),
         height    = as.numeric(s$height_i))
}
zv <- lapply(all_res, zone_vals)

lu_vars <- c(household = "(世帯)", rent = "(円/月)", floor = "(m^2)", Gi = "(m^2)", height = "(m)")
lu_titles <- c(household = "居住世帯数", rent = "平均地代", floor = "平均床面積",
               Gi = "宅地面積", height = "推定建物高さ")

for (v in names(lu_vars)) {
  d <- key_code_sf %>%
    left_join(zv$scn0 %>% dplyr::select(KEY_CODE, base = all_of(v)), by = "KEY_CODE") %>%
    left_join(zv$emp2009 %>% dplyr::select(KEY_CODE, y09 = all_of(v)), by = "KEY_CODE") %>%
    left_join(zv$emp2021 %>% dplyr::select(KEY_CODE, y21 = all_of(v)), by = "KEY_CODE") %>%
    mutate(d_21_09 = y21 - y09, d_09_0 = y09 - base, d_21_0 = y21 - base)

  p1 <- make_diff_map2(d, "d_21_09", paste0(lu_titles[v], "：2021 − 2009"), lu_vars[v])
  print(p1)
  ggsave(sprintf("%s/landuse/%s_diff_2021_2009.png", emp_plot_dir, v), p1, width = 7, height = 6, dpi = 150)

  p2 <- make_diff_map2(d, c("d_09_0", "d_21_0"),
                       paste0(lu_titles[v], c("：2009 − 現状", "：2021 − 現状")), lu_vars[v])
  print(p2)
  ggsave(sprintf("%s/landuse/%s_diff_vs_scn0.png", emp_plot_dir, v), p2, width = 12, height = 6, dpi = 150)
}

#道路・鉄道 差分マップ（2021 − 2009）####
make_link_diff_map <- function(map_data, label, unit, title) {
  lim <- max(abs(map_data$d), na.rm = TRUE)
  ggplot(map_data) +
    geom_sf(aes(linewidth = abs(d), color = d)) +
    scale_linewidth_continuous(range = c(0.4, 2.8), limits = c(0, lim), transform = "sqrt",
                               name = paste0(label, "の変化\n(|Δ ", unit, "|)")) +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                          limits = c(-lim, lim), oob = scales::squish,
                          name = paste0(label, "の変化\n(Δ ", unit, ")")) +
    labs(title = title) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

road09 <- build_scn_link_map_data(emp_results$emp2009)
road21 <- build_scn_link_map_data(emp_results$emp2021)
road_diff <- build_scn_diff_map_data(road21, road09)

p_flow <- make_link_diff_map(road_diff %>% mutate(d = flow_diff), "交通量", "pcu/日", "道路交通量：2021 − 2009")
print(p_flow)
ggsave(file.path(emp_plot_dir, "road/flow_diff_2021_2009.png"), p_flow, width = 7, height = 6, dpi = 150)

p_speed <- make_link_diff_map(road_diff %>% mutate(d = speed_diff), "速度", "km/h", "道路速度：2021 − 2009")
print(p_speed)
ggsave(file.path(emp_plot_dir, "road/speed_diff_2021_2009.png"), p_speed, width = 7, height = 6, dpi = 150)

rail_diff <- build_scn_rail_diff_map_data(build_scn_rail_map_data(emp_results$emp2021),
                                          build_scn_rail_map_data(emp_results$emp2009))
p_rail <- make_link_diff_map(rail_diff %>% mutate(d = flow_diff), "鉄道利用世帯数", "世帯/日",
                             "鉄道利用世帯数：2021 − 2009")
print(p_rail)
ggsave(file.path(emp_plot_dir, "rail/rail_flow_diff_2021_2009.png"), p_rail, width = 7, height = 6, dpi = 150)

#総合指標（メインの「総合指標の表」と同じ式）####
ev_benefit <- function(base, scn) {
  dv <- matrix(exp(scn$v) - exp(base$v), nrow = nz_res, ncol = nz_work, byrow = TRUE)
  ev <- (1 / alpha_0) * base$state$r_ij_H^alpha_a * dv
  sum((base$state$l_i_j + scn$state$l_i_j) / 2 * ev, na.rm = TRUE) / 10  # 万円/月
}
indicators <- function(res) {
  s <- res$state
  co2_car  <- sum(s$l_i_j * res$P.car  * dist_car.km  * 2, na.rm = TRUE) * eCO2_car  / 1e6
  co2_rail <- sum(s$l_i_j * res$P.rail * dist_rail.km * 2, na.rm = TRUE) * eCO2_rail / 1e6
  total_ar <- sum(s$a_fij_H * s$l_i_j, na.rm = TRUE)
  co2_house <- (total_ar * 296 / 3.6 / 4.17 + (1193 * 1.99)) * 0.57 / 1000 / 365
  c(co2_car = co2_car, co2_rail = co2_rail, co2_house = co2_house,
    co2_total = co2_car + co2_rail + co2_house,
    land_rent = sum(s$rg_i * s$G_i + (G0_i - s$G_i) * rr_a) / 1e5,
    res_land  = sum(s$G_i) / 1e6,
    floor     = total_ar / 1e6)
}
ind <- sapply(all_res, indicators)
ind_lab  <- c("自動車CO2排出量", "鉄道CO2排出量", "住宅CO2排出量", "CO2排出量 合計",
              "総地代収入", "宅地面積", "総住宅床面積")
ind_unit <- c("tCO2/日", "tCO2/日", "tCO2/日", "tCO2/日", "億円/月", "km²", "km²")
ind_dig  <- c(1, 1, 1, 1, 2, 3, 3)
fn <- function(x, d) formatC(x, format = "f", digits = d, big.mark = ",")
fd <- function(x, x0, d) sprintf("%s%s（%+.2f%%）", ifelse(x >= x0, "+", ""), fn(x - x0, d), (x - x0) / x0 * 100)
ind_rows <- sapply(seq_len(nrow(ind)), function(k) {
  v <- ind[k, ]; d <- ind_dig[k]
  sprintf("| %s | %s | %s | %s | %s | %s |", ind_lab[k], ind_unit[k],
          fn(v["scn0"], d), fn(v["emp2009"], d), fn(v["emp2021"], d), fd(v["emp2021"], v["emp2009"], d))
})
ben <- c(b09 = ev_benefit(all_res$scn0, all_res$emp2009),
         b21 = ev_benefit(all_res$scn0, all_res$emp2021),
         b21_09 = ev_benefit(all_res$emp2009, all_res$emp2021))

writeLines(c(
  "# 2009/2021 雇用分布シナリオ 総合指標（自動生成）",
  "",
  paste0("- active_calibration: `", active_calibration, "`"),
  paste0("- 実行日時: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "（同じ実行の記録は `run_info.md`）"),
  "",
  "| 指標 | 単位 | 現状 | 2009年雇用分布 | 2021年雇用分布 | 2021 − 2009 |",
  "|---|---|--:|--:|--:|--:|",
  ind_rows,
  "",
  "## 便益（EV、万円/月）",
  "",
  sprintf("- 2009年雇用分布 − 現状: %s", fn(ben["b09"], 0)),
  sprintf("- 2021年雇用分布 − 現状: %s", fn(ben["b21"], 0)),
  sprintf("- 2021年 − 2009年（2009を基準としたEV）: %s", fn(ben["b21_09"], 0))
), file.path(emp_run_dir, "summary_indicators.md"))

#実行記録####
git_commit <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) NA)
git_dirty  <- tryCatch(length(system("git status --porcelain -- scripts", intern = TRUE)) > 0,
                       error = function(e) NA)
writeLines(c(
  "# 2009/2021 雇用分布シナリオ 実行記録（自動生成）",
  "",
  paste0("- 実行日時: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  paste0("- active_calibration: `", active_calibration, "`"),
  paste0("- git commit: ", git_commit, ifelse(isTRUE(git_dirty), "（scripts/ に未コミットの変更あり）", "")),
  paste0("- 揃えた総従業者数（scn0）: ", fn(total_L0, 0)),
  paste0("- 揃えた総給与 Σω·L（scn0）: ", fn(payroll0, 0)),
  paste0("- LUTIループ: msa = ", emp_msa, ", max_iter = 50, tol = 100"),
  "",
  "| シナリオ | ボロノイ集計の総数（スケール前） | 従業者数倍率 | 賃金倍率 | Σω·L（調整後） | converged | iterations | final_diff |",
  "|---|--:|--:|--:|--:|---|--:|--:|",
  sapply(names(emp_years), function(y) {
    e <- emp_years[[y]]; r <- emp_results[[y]]
    sprintf("| %s | %s | %.4f | %.4f | %s | %s | %d | %.3g |", y, fn(e$raw_total, 0), e$scale, e$wage_k,
            fn(sum(omega_j$omega_j * e$wage_k * e$L_j_hat$L_j_hat, na.rm = TRUE), 0),
            r$converged, r$iterations, r$final_diff)
  })
), file.path(emp_run_dir, "run_info.md"))
