# 中心地 K=24/32/46（17拠点＋2021年に単独E人以上のメッシュ）の道路（混雑なし）・鉄道・自転車の所要時間行列を作る
# 作り方は今の dists0.xdr / railway_dist.xdr / dists0.bike.xdr と同じ（reproduce_17.R・02 の bike_check.R で再現を確認）
# 出力：data/processed/centers_E<E>/（Git管理外）
suppressMessages({library(sf);library(dplyr);library(tidyr);library(cppRouting)})
load("data/osm/link10.xdr"); load("data/osm/node10.xdr"); load("data/work_zone.xdr")
load("data/dists0.xdr"); zones_all <- rownames(dists0)
load("data/osm/rail_network.xdr"); load("data/osm/rail_network.nodes.xdr")
e21 <- read.csv("data/raw/経済センサス2021/tblT001146S5030.csv", fileEncoding="CP932", stringsAsFactors=FALSE, na.strings=c("","NA")) %>%
  filter(!is.na(KEY_CODE)) %>% transmute(KEY_CODE=as.character(KEY_CODE), emp=replace_na(suppressWarnings(as.numeric(T001146022)),0))
wz17 <- as.character(work_zone)   # 18（50302329 はメインで道路網の孤立により除外される）
nodes <- data.frame(node10$ID, st_coordinates(node10)); names(nodes) <- c("Node","X","Y")
mk <- function(sp) makegraph(df = (link10 %>% mutate(FFtime = linklen/sp*60/1e3))[,c("ID1","ID2","FFtime")] %>% st_drop_geometry(), directed=TRUE, coords=nodes)
g_road <- makegraph(df = (link10 %>% mutate(FFtime = linklen/maxspeed*60/1e3))[,c("ID1","ID2","FFtime")] %>% st_drop_geometry(), directed=TRUE, coords=nodes)
g_bike <- mk(15)
lk <- data.frame(type=c("rail","subway","access"), speed=c(60,40,5))
lr <- rbind(rail_network, rail_network %>% rename(ID1=ID2, ID2=ID1)) %>% left_join(lk, by="type") %>% mutate(FFtime=length/speed*60/1e3)
nr <- data.frame(rail_network.nodes$nodeID, st_coordinates(rail_network.nodes)); names(nr) <- c("Node","X","Y")
g_rail <- makegraph(df = lr[,c("ID1","ID2","FFtime")] %>% st_drop_geometry(), directed=TRUE, coords=nr)
for (E in c(20000, 10000, 5000)) {
  cen <- sort(union(wz17, e21$KEY_CODE[e21$emp >= E & paste0("mc_", e21$KEY_CODE) %in% zones_all])); ce <- paste0("mc_", cen)
  stopifnot(all(ce %in% nodes$Node), all(ce %in% nr$Node))
  road <- get_distance_matrix(g_road, from = zones_all, to = ce, algorithm = "mch")
  bike <- get_distance_matrix(g_bike, from = zones_all, to = ce, algorithm = "mch")
  zr <- zones_all[zones_all %in% nr$Node]
  rail <- get_distance_matrix(g_rail, from = zr, to = ce, algorithm = "mch")
  out <- file.path("data/processed", paste0("centers_E", E)); dir.create(out, recursive = TRUE, showWarnings = FALSE)
  work_zone_new <- cen
  dists0 <- road; save(dists0, file = file.path(out, "dists0.xdr"))
  dists0.bike <- bike; save(dists0.bike, file = file.path(out, "dists0.bike.xdr"))
  dists0 <- rail; save(dists0, file = file.path(out, "railway_dist.xdr"))
  save(work_zone_new, file = file.path(out, "work_zone.xdr"))
  cat(sprintf("E=%d: 中心地 %d（うち17拠点系 %d）、道路NA %d、自転車NA %d、鉄道NA %d、各中心地の道路NA数の最大 %d\n",
              E, length(cen), length(wz17), sum(is.na(road)), sum(is.na(bike)), sum(is.na(rail)), max(colSums(is.na(road)))))
}
