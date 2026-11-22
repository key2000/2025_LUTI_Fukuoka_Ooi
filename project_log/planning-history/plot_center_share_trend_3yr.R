# 中心地ごとの従業者数とシェアの変化（2009→2014→2021の3時点）の表と地図。記録：project_log/planning-history/centers_history.md
# 実行（リポジトリルート）：Rscript project_log/planning-history/plot_center_share_trend_3yr.R
suppressMessages({library(sf); library(dplyr); library(ggplot2); library(patchwork)})
load("data/key_code_sf.xdr")
ks <- key_code_sf %>% mutate(KEY_CODE = as.character(KEY_CODE))
nm <- c(`50303302`="天神・博多", `50303393`="アイランドシティ", `50302290`="九大学研都市", `50305318`="福間",
        `50302176`="前原", `50303395`="香椎", `50303206`="姪浜", `50301491`="二日市", `50304377`="古賀",
        `50303344`="箱崎", `50302422`="太宰府", `50305475`="赤間", `50302347`="春日原", `50302390`="六本松",
        `50303208`="西新", `50302364`="大橋", `50302356`="雑餉隈")
inner <- c("50303302", "50303208", "50302390", "50302364", "50302356", "50302347", "50303206")
cz <- st_coordinates(st_centroid(st_geometry(ks %>% filter(KEY_CODE == "50303302"))))

trend_tab <- function(dir, assign) {
  load(file.path("data/processed", dir, paste0("inputs_", assign, ".xdr")), envir = environment())
  L <- sapply(L_by_year, function(x) x$L_j_hat); s <- sweep(L, 2, colSums(L), "/")
  list(d = data.frame(center = rownames(L_by_year$`2021`), name = unname(nm[rownames(L_by_year$`2021`)]),
                      L09 = L[, "2009"], L14 = L[, "2014"], L21 = L[, "2021"],
                      g0914 = (L[, 2] / L[, 1] - 1) * 100, g1421 = (L[, 3] / L[, 2] - 1) * 100,
                      g0921 = (L[, 3] / L[, 1] - 1) * 100,
                      s0914 = (s[, 2] / s[, 1] - 1) * 100, s1421 = (s[, 3] / s[, 2] - 1) * 100,
                      s0921 = (s[, 3] / s[, 1] - 1) * 100),
       assignment = assignment, total = colSums(L))
}

share_map <- function(tt, col, title, zoom = FALSE) {
  d <- tt$d %>% mutate(chg = .data[[col]])
  reg <- ks %>% inner_join(tt$assignment, by = "KEY_CODE") %>% group_by(center) %>% summarise(.groups = "drop") %>%
    left_join(d, by = "center")
  pt <- st_centroid(ks %>% filter(KEY_CODE %in% d$center)) %>% left_join(d, by = c("KEY_CODE" = "center"))
  lab <- pt %>% filter(KEY_CODE %in% names(nm)) %>% mutate(lab = sprintf("%s\n%+.0f%%", nm[KEY_CODE], chg))
  xy <- st_coordinates(lab)
  lab <- if (zoom) lab[abs(xy[, 1] - cz[1]) < 9000 & xy[, 2] > cz[2] - 9000 & xy[, 2] < cz[2] + 5000, ]
         else lab %>% filter(!KEY_CODE %in% inner)
  lim <- 30
  ggplot() +
    geom_sf(data = reg, aes(fill = pmax(pmin(chg, lim), -lim)), color = "grey40", linewidth = 0.2) +
    geom_sf(data = pt, size = 0.8) +
    geom_sf_text(data = lab, aes(label = lab), size = if (zoom) 2.8 else 2.3, family = "Hiragino Sans",
                 lineheight = 0.85, nudge_y = if (zoom) 700 else 1500) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", limits = c(-lim, lim),
                         name = "シェアの\n変化(%)") +
    (if (zoom) coord_sf(xlim = cz[1] + c(-9000, 9000), ylim = cz[2] + c(-9000, 5000), expand = FALSE) else NULL) +
    labs(title = title) + theme_void(base_family = "Hiragino Sans") +
    theme(plot.background = element_rect(fill = "white", color = NA), plot.title = element_text(size = 10))
}

t17 <- trend_tab("centers_orig17", "voronoi"); t32 <- trend_tab("centers_E10000", "time")
gr <- function(tt) sprintf("（都市圏計 %+.1f%%）", (tt$total[-1] / tt$total[-3] - 1) * 100)
g17 <- gr(t17)
p <- (share_map(t17, "s0914", paste("17拠点・ボロノイ 2009→2014", g17[1])) +
      share_map(t17, "s1421", paste("17拠点・ボロノイ 2014→2021", g17[2]))) /
     (share_map(t32, "s0914", "中心地32・所要時間 2009→2014") +
      share_map(t32, "s1421", "中心地32・所要時間 2014→2021")) /
     (share_map(t32, "s0914", "（拡大）中心地32：都心のまわり 2009→2014", zoom = TRUE) +
      share_map(t32, "s1421", "（拡大）中心地32：都心のまわり 2014→2021", zoom = TRUE)) +
  plot_layout(guides = "collect", heights = c(1, 1, 0.85)) + plot_annotation(
    title = "従業者数のシェアの変化 3時点（左 2009→2014、右 2014→2021。都市圏計より伸びた＝赤、伸びが小さい＝青）",
    caption = paste("シェアの変化 = (後の年のシェア / 前の年のシェア − 1)。経済センサス1kmメッシュ（2009・2014 基礎調査、2021 活動調査）を各分割で集計。",
                    "±30%で色を頭打ち（アイランドシティ 2014→2021 は +62%〜+71%）。下段は天神・博多のまわりの拡大", sep = "\n"),
    theme = theme(text = element_text(family = "Hiragino Sans"), plot.background = element_rect(fill = "white", color = NA)))
plot(p)
ggsave("project_log/planning-history/plots/center_share_change_2009_2014_2021.png", p, width = 13, height = 17,
       dpi = 200, device = ragg::agg_png, bg = "white")

for (x in list(list(t17, "17_voronoi"), list(t32, "32_time")))
  write.csv(x[[1]]$d %>% mutate(across(where(is.double), ~round(., 1))),
            sprintf("project_log/planning-history/plots/center_employment_3yr_%s.csv", x[[2]]), row.names = FALSE,
            fileEncoding = "UTF-8")
print(lapply(list(t17 = t17$total, t32 = t32$total), identity))
