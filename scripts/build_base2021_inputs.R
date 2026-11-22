# 基準年2021・中心地を増やしたモデルの入力（中心地・従業者数 L_j_hat・賃金 omega_j）を作る
# 記録：project_log/base-year-revision/09_base2021-inputs/notes.md
# 実行（リポジトリルート）：Rscript scripts/build_base2021_inputs.R
# 前提：project_log/base-year-revision/08_matrices-32/build_matrices.R で所要時間行列を作成済み
# 出力：data/processed/centers_E<E>/inputs_<assign>.xdr（assign = time / voronoi）
suppressMessages({library(sf); library(dplyr); library(tidyr); library(cppRouting)})

load("data/key_code_sf.xdr"); load("data/work_zone.xdr")
load("data/osm/link10.xdr"); load("data/osm/node10.xdr")

rd <- function(p, cols) {
  x <- read.csv(p, fileEncoding = "CP932", stringsAsFactors = FALSE, na.strings = c("", "NA"))
  x <- x[-1, ] %>% filter(!is.na(KEY_CODE))
  x %>% transmute(KEY_CODE = as.character(KEY_CODE),
                  across(all_of(unname(cols)), ~replace_na(suppressWarnings(as.numeric(.)), 0)))
}
emp_y <- list(
  `2009` = rd("data/raw/経済センサス2009/tblT000616S5030.csv", "T000616002") %>% rename(emp = T000616002),
  `2014` = rd("data/raw/経済センサス従業員数1kmメッシュ/tblT000841S5030.csv", "T000841002") %>% rename(emp = T000841002),
  `2021` = rd("data/raw/経済センサス2021/tblT001146S5030.csv", "T001146022") %>% rename(emp = T001146022))

# 2021 産業別従業者数（T001157、022=A〜S, 023=A〜R, 025〜041=C〜R, 042=S）
ind_cols <- c(C = "T001157025", D = "T001157026", E = "T001157027", F = "T001157029", G = "T001157030",
              H = "T001157031", I = "T001157032", J = "T001157033", K = "T001157034", L = "T001157035",
              M = "T001157036", N = "T001157037", O = "T001157038", P = "T001157039", Q = "T001157040",
              R = "T001157041")
ind21 <- rd("data/raw/経済センサス　1kmメッシュ　産業別従業者数/tblT001157S5030.csv",
            c("T001157022", "T001157023", ind_cols, "T001157042"))
ind21 <- ind21 %>% transmute(KEY_CODE, AS = T001157022,
                             AB = pmax(T001157023 - rowSums(across(all_of(unname(ind_cols)))), 0),
                             across(all_of(unname(ind_cols))), S = T001157042)
names(ind21)[match(ind_cols, names(ind21))] <- names(ind_cols)

# 産業別賃金（06_wage-by-year、きまって支給する現金給与額、千円/月）。A・B・S は産業計で補う
wage <- read.csv("data/raw/賃金構造基本統計調査　福岡　産業別/Fukuoka_avg_wage_by_year.csv", stringsAsFactors = FALSE)
wage_vec <- function(y) {
  w <- wage %>% filter(year == y); v <- setNames(w$avg_wage, w$Industry_id)
  c(AB = v[["ALL"]], v[names(ind_cols)], S = v[["ALL"]])
}

ks <- key_code_sf %>% mutate(KEY_CODE = as.character(KEY_CODE))
mp <- st_centroid(ks)
meshes <- ks$KEY_CODE
nodes <- data.frame(node10$ID, st_coordinates(node10)); names(nodes) <- c("Node", "X", "Y")
g_road <- makegraph(df = (link10 %>% mutate(FFtime = linklen / maxspeed * 60 / 1e3))[, c("ID1", "ID2", "FFtime")] %>%
                      st_drop_geometry(), directed = TRUE, coords = nodes)

assign_voronoi <- function(cen) {
  cp <- st_centroid(ks %>% filter(KEY_CODE %in% cen))
  vor <- cp %>% st_geometry() %>% st_union() %>% st_voronoi() %>% st_collection_extract("POLYGON") %>% st_sf()
  vor <- st_join(vor, cp %>% dplyr::select(center = KEY_CODE), join = st_intersects)
  st_join(mp %>% dplyr::select(KEY_CODE), vor, join = st_within) %>% st_drop_geometry()
}
assign_time <- function(cen) {
  tm <- get_distance_matrix(g_road, from = paste0("mc_", meshes), to = paste0("mc_", cen), algorithm = "mch")
  best <- apply(tm, 1, function(x) if (all(is.na(x))) NA_integer_ else which.min(x))
  a <- data.frame(KEY_CODE = sub("mc_", "", rownames(tm)), center = sub("mc_", "", colnames(tm))[best])  # 行列の並びは入力順と限らない
  a <- a[match(meshes, a$KEY_CODE), ]
  miss <- is.na(a$center)                                   # 道路でどの中心地にも行けないメッシュは直線で
  if (any(miss)) a$center[miss] <- assign_voronoi(cen)$center[match(a$KEY_CODE[miss], assign_voronoi(cen)$KEY_CODE)]
  attr(a, "n_fallback") <- sum(miss)
  a
}

# 道路網で孤立している中心地（今の 50302329）は除く（メインの L.311〜356 と同じ扱い）
load("data/dists0.xdr"); iso <- sub("mc_", "", colnames(dists0)[colSums(!is.na(dists0)) <= 1])

for (E in c(Inf, 20000, 10000, 5000)) {
  cen <- union(as.character(work_zone), emp_y$`2021`$KEY_CODE[emp_y$`2021`$emp >= E & emp_y$`2021`$KEY_CODE %in% meshes])
  cen <- sort(setdiff(cen, iso))
  out <- file.path("data/processed", if (is.infinite(E)) "centers_orig17" else paste0("centers_E", E))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  for (am in c("time", "voronoi")) {
    a <- if (am == "time") assign_time(cen) else assign_voronoi(cen)
    L_by_year <- lapply(emp_y, function(e) {
      x <- a %>% left_join(e, by = "KEY_CODE") %>% mutate(emp = replace_na(emp, 0)) %>%
        group_by(center) %>% summarise(L_j_hat = sum(emp), .groups = "drop")
      x <- data.frame(L_j_hat = x$L_j_hat[match(cen, x$center)], row.names = cen); x$L_j_hat[is.na(x$L_j_hat)] <- 0; x
    })
    mix <- a %>% left_join(ind21, by = "KEY_CODE") %>% mutate(across(-c(KEY_CODE, center), ~replace_na(., 0))) %>%
      group_by(center) %>% summarise(across(-KEY_CODE, sum), .groups = "drop")
    mix <- mix[match(cen, mix$center), ]
    share <- as.matrix(mix[, c("AB", names(ind_cols), "S")]); share <- share / rowSums(share)
    omega_by_year <- lapply(c(`2009` = 2009, `2014` = 2014, `2021` = 2021), function(y)
      data.frame(KEY_CODE = cen, omega_j = as.vector(share %*% wage_vec(y))))
    work_zone_new <- cen; L_j_hat <- L_by_year$`2021`; omega_j <- omega_by_year$`2021`
    assignment <- a
    save(work_zone_new, L_j_hat, omega_j, L_by_year, omega_by_year, assignment, share,
         file = file.path(out, paste0("inputs_", am, ".xdr")))
    cat(sprintf("%s %s: 中心地 %d、2021総従業者 %.0f、ω_j(2021) %.1f〜%.1f、直線で補ったメッシュ %s\n",
                basename(out), am, length(cen), sum(L_j_hat$L_j_hat), min(omega_j$omega_j), max(omega_j$omega_j),
                ifelse(is.null(attr(a, "n_fallback")), "-", attr(a, "n_fallback"))))
  }
}
