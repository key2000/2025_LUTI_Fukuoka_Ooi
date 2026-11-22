# 中心地ごとの雇用シェアの変化（2009→2021）の地図。記録：project_log/planning-history/centers_history.md
# 実行（リポジトリルート）：Rscript project_log/planning-history/plot_center_share_trend.R
suppressMessages({library(sf); library(dplyr); library(ggplot2); library(patchwork)})
load("data/key_code_sf.xdr")
ks <- key_code_sf %>% mutate(KEY_CODE = as.character(KEY_CODE))
nm <- c(`50303302`="天神・博多", `50303393`="アイランドシティ", `50302290`="九大学研都市", `50305318`="福間",
        `50302176`="前原", `50303395`="香椎", `50303206`="姪浜", `50301491`="二日市", `50304377`="古賀",
        `50303344`="箱崎", `50302422`="太宰府", `50305475`="赤間", `50302347`="春日原", `50302390`="六本松",
        `50303208`="西新", `50302364`="大橋", `50302356`="雑餉隈")

share_map <- function(dir, assign, title, zoom = FALSE) {
  load(file.path("data/processed", dir, paste0("inputs_", assign, ".xdr")))
  L09 <- L_by_year$`2009`$L_j_hat; L21 <- L_by_year$`2021`$L_j_hat
  d <- data.frame(center = rownames(L_by_year$`2021`),
                  chg = ((L21 / sum(L21)) / (L09 / sum(L09)) - 1) * 100)   # 都市圏計に対する伸びの差（%）
  reg <- ks %>% inner_join(assignment, by = "KEY_CODE") %>% group_by(center) %>% summarise(.groups = "drop") %>%
    left_join(d, by = "center")
  pt <- st_centroid(ks %>% filter(KEY_CODE %in% d$center)) %>% left_join(d, by = c("KEY_CODE" = "center"))
  lab <- pt %>% filter(KEY_CODE %in% names(nm)) %>% mutate(lab = sprintf("%s\n%+.0f%%", nm[KEY_CODE], chg))
  lim <- 30
  cz <- st_coordinates(st_centroid(st_geometry(ks %>% filter(KEY_CODE == "50303302"))))
  if (zoom) lab <- lab %>% filter(st_coordinates(.)[, 1] > cz[1] - 9000 & st_coordinates(.)[, 1] < cz[1] + 9000 &
                                     st_coordinates(.)[, 2] > cz[2] - 9000 & st_coordinates(.)[, 2] < cz[2] + 5000)
  else lab <- lab %>% filter(!KEY_CODE %in% c("50303302", "50303208", "50302390", "50302364", "50302356", "50302347", "50303206"))
  ggplot() +
    geom_sf(data = reg, aes(fill = pmax(pmin(chg, lim), -lim)), color = "grey40", linewidth = 0.2) +
    geom_sf(data = pt, size = 0.8) +
    geom_sf_text(data = lab, aes(label = lab), size = if (zoom) 3 else 2.4, family = "Hiragino Sans", lineheight = 0.85,
                 nudge_y = if (zoom) 700 else 1500) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", limits = c(-lim, lim),
                         name = "シェアの\n変化(%)") +
    (if (zoom) coord_sf(xlim = cz[1] + c(-9000, 9000), ylim = cz[2] + c(-9000, 5000), expand = FALSE) else NULL) +
    labs(title = title) + theme_void(base_family = "Hiragino Sans") +
    theme(plot.background = element_rect(fill = "white", color = NA), plot.title = element_text(size = 10))
}
p1 <- share_map("centers_orig17", "voronoi", "17拠点・ボロノイ（修論と同じ分割）")
p2 <- share_map("centers_E10000", "time", "中心地32・道路の所要時間（新しい設定）")
p3 <- share_map("centers_orig17", "voronoi", "（拡大）17拠点・ボロノイ：都心のまわり", zoom = TRUE)
p4 <- share_map("centers_E10000", "time", "（拡大）中心地32・所要時間：都心のまわり", zoom = TRUE)
p <- (p1 + p2) / (p3 + p4) + plot_layout(guides = "collect", heights = c(1, 0.9)) + plot_annotation(
  title = "従業者数のシェアの変化 2009→2021（都市圏計 +6.7% より伸びた＝赤、伸びが小さい＝青）",
  caption = "シェアの変化 = (2021シェア / 2009シェア − 1)。経済センサス1kmメッシュを各分割で集計。±30%で色を頭打ち（アイランドシティは +78%〜+86%）。下段は天神・博多のまわりの拡大",
  theme = theme(text = element_text(family = "Hiragino Sans"), plot.background = element_rect(fill = "white", color = NA)))
plot(p)
ggsave("project_log/planning-history/plots/center_share_change_2009_2021.png", p, width = 13, height = 12,
       dpi = 200, device = ragg::agg_png, bg = "white")
