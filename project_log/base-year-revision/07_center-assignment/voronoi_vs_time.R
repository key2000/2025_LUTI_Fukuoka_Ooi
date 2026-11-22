# 中心地への集約：直線距離（ボロノイ）と道路の所要時間（混雑なし）で、割り当てがどれだけ違うか
# 実行（リポジトリルート）: Rscript project_log/base-year-revision/07_center-assignment/voronoi_vs_time.R
suppressMessages({library(sf); library(dplyr); library(tidyr); library(cppRouting)})
load("data/key_code_sf.xdr"); load("data/work_zone.xdr")
load("data/osm/link10.xdr"); load("data/osm/node10.xdr")
rd <- function(p, col) read.csv(p, fileEncoding = "CP932", stringsAsFactors = FALSE, na.strings = c("", "NA")) %>%
  filter(!is.na(KEY_CODE)) %>% transmute(KEY_CODE = as.character(KEY_CODE), emp = replace_na(suppressWarnings(as.numeric(.data[[col]])), 0))
e21 <- rd("data/raw/経済センサス2021/tblT001146S5030.csv", "T001146022")
ks <- key_code_sf %>% mutate(KEY_CODE = as.character(KEY_CODE))
mp <- st_centroid(ks)
d <- st_drop_geometry(mp) %>% select(KEY_CODE) %>% left_join(e21, by = "KEY_CODE") %>% mutate(emp = replace_na(emp, 0))
wz17 <- setdiff(as.character(work_zone), "50302329")

link10 <- link10 %>% mutate(FFtime = linklen / maxspeed * 60 / 10^3)
nodes <- data.frame(node10$ID, st_coordinates(node10)); names(nodes) <- c("Node", "X", "Y")
g <- makegraph(link10[, c("ID1", "ID2", "FFtime")] %>% st_drop_geometry(), directed = TRUE, coords = nodes)
mesh_nodes <- paste0("mc_", d$KEY_CODE)
ok_nodes <- mesh_nodes[mesh_nodes %in% g$dict$ref]
cat("メッシュ数", nrow(d), " 道路網につながるメッシュ", length(ok_nodes), "\n")

vor_assign <- function(centers) {
  cp <- st_centroid(ks %>% filter(KEY_CODE %in% centers))
  vor <- cp %>% st_geometry() %>% st_union() %>% st_voronoi() %>% st_collection_extract("POLYGON") %>% st_sf()
  vor <- st_join(vor, cp %>% select(center = KEY_CODE), join = st_intersects)
  st_join(mp %>% select(KEY_CODE), vor, join = st_within) %>% st_drop_geometry()
}
res <- list(); detail <- list()
for (E in c(Inf, 20000, 10000, 5000)) {
  cen <- union(wz17, d$KEY_CODE[d$emp >= E])
  cen_nodes <- paste0("mc_", cen); cen_nodes <- cen_nodes[cen_nodes %in% g$dict$ref]
  tm <- get_distance_matrix(g, from = ok_nodes, to = cen_nodes, algorithm = "mch")
  near_t <- data.frame(KEY_CODE = sub("mc_", "", rownames(tm)),
                       center_t = sub("mc_", "", colnames(tm))[apply(tm, 1, function(x) if (all(is.na(x))) NA else which.min(x))],
                       t_min = apply(tm, 1, function(x) suppressWarnings(min(x, na.rm = TRUE))))
  a <- d %>% left_join(vor_assign(cen), by = "KEY_CODE") %>% left_join(near_t, by = "KEY_CODE") %>%
    filter(!is.na(center_t), !is.na(center))
  a$t_vor <- mapply(function(k, c) tm[paste0("mc_", k), paste0("mc_", c)], a$KEY_CODE, a$center)
  diffm <- a$center != a$center_t
  res[[as.character(E)]] <- data.frame(E = E, K = length(cen), meshes = nrow(a), meshes_changed = sum(diffm),
    share_mesh = round(100 * mean(diffm), 1), emp = sum(a$emp), emp_changed = sum(a$emp[diffm]),
    share_emp = round(100 * sum(a$emp[diffm]) / sum(a$emp), 1),
    extra_min_med = round(median((a$t_vor - a$t_min)[diffm], na.rm = TRUE), 1),
    extra_min_max = round(max((a$t_vor - a$t_min)[diffm], na.rm = TRUE), 1))
  zt <- a %>% group_by(center) %>% summarise(L_vor = sum(emp)) %>%
    full_join(a %>% group_by(center = center_t) %>% summarise(L_time = sum(emp)), by = "center") %>%
    mutate(across(c(L_vor, L_time), ~replace_na(., 0)), d = L_time - L_vor, d_pct = round(100 * d / L_vor, 1)) %>% arrange(desc(abs(d)))
  detail[[as.character(E)]] <- zt
}
r <- bind_rows(res); print(r)
cat("\n== E=1万：中心地ごとの従業者数（直線 vs 所要時間）、差の大きい順 ==\n"); print(as.data.frame(head(detail[["10000"]], 15)))
cat("\n== 今の17：同 ==\n"); print(as.data.frame(head(detail[["Inf"]], 17)))
saveRDS(list(summary = r, detail = detail), "project_log/base-year-revision/07_center-assignment/voronoi_vs_time.rds")
