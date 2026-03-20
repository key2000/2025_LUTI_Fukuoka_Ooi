# 路線価データ
#####
rm(list=ls())
gc();gc();

library(sf)
library(dplyr)

target_crs <- 6670
load("data/PT_bnd3.xdr") # PT_bnd3
PT_bnd3.wgs=st_transform(PT_bnd3,4326)
st_write(PT_bnd3.wgs,dsn="data/processed/PT_bnd3.wgs.gpkg")
bbox=st_bbox(PT_bnd3.wgs)

infile  <- "H:/data/NII_IDR/REX地価コンテンツデータセット/data/REX_data_2020-2022/2022/nouhin_line_2022.shp"
outfile <- "data/processed/REX2022.gpkg"  # 出力は GPKG 推奨（Shapefileより堅牢）

# xmin, ymin, xmax, ymax（元データの座標系に合わせること）
# bbox <- c(135.0, 34.5, 135.8, 35.0)

gdal_utils(
  util = "vectortranslate",
  source = infile,
  destination = outfile,
  options = c(
    "-f", "GPKG",
    "-clipsrc", as.character(bbox[1]), as.character(bbox[2]),
    as.character(bbox[3]), as.character(bbox[4])
  ),
  quiet = TRUE
)

sub <- st_read(outfile, quiet = TRUE)
st_crs(sub)=4326
st_write(sub,dsn="data/processed/REX2022_4326.gpkg")

#####


# 公示地価データ
#####

# target_crs <- 6668
PT_bnd3.6668=st_transform(PT_bnd3,6668)
bbox=st_bbox(PT_bnd3.6668)

infile  <- "E:/WorkDir01/data/GIS/Japan/MLIT/ksj/landPrice/bulk_2025/L01-25_GML/L01-25_GML/L01-25.shp"
outfile <- "data/processed/KSJ_L01-25.gpkg"  # 出力は GPKG 推奨（Shapefileより堅牢）

infile  <- "E:/WorkDir01/data/GIS/Japan/MLIT/ksj/landPrice/bulk_2025/L02-25_GML/L02-25.shp"
outfile <- "data/processed/KSJ_L02-25.gpkg"  # 出力は GPKG 推奨（Shapefileより堅牢）

gdal_utils(
  util = "vectortranslate",
  source = infile,
  destination = outfile,
  options = c(
    "-f", "GPKG",
    "-clipsrc", as.character(bbox[1]), as.character(bbox[2]),
    as.character(bbox[3]), as.character(bbox[4])
  ),
  quiet = TRUE
)


#####


#####
rm(list=ls())
gc();gc();

library(sf)
library(dplyr)

load("data/PT_bnd3.xdr") # PT_bnd3
PT_bnd3.6668=st_transform(PT_bnd3,6668)

KSJ_L01_25=st_read("data/processed/KSJ_L01-25.gpkg")  # 出力は GPKG 推奨（Shapefileより堅牢）
KSJ_L02_25=st_read("data/processed/KSJ_L02-25.gpkg")  # 出力は GPKG 推奨（Shapefileより堅牢）

# （単位は[円/㎡]）
tL01=KSJ_L01_25 %>% select(price=L01_008)
tL02=KSJ_L02_25 %>% filter(L02_003!="020") %>% select(price=L02_006)
tL_price=rbind(tL01,tL02)
tI01=st_intersects(PT_bnd3.6668,tL_price)

# tI01[4][1] %>% is_empty()
avgPrice=lapply(tI01,function(x){ifelse(length(x)==0,NA,mean(tL_price$price[x]))}) %>% unlist()

PT_bnd3.6668=PT_bnd3.6668 %>% mutate(avgPrice=avgPrice)

library(ggplot2)
p=ggplot(PT_bnd3.6668) +
  geom_sf(aes(fill = log(avgPrice)), color = NA) +
  scale_fill_viridis_c(option = "C", direction = 1, name = "指標（対数）") +
  labs(title = "price") +
  theme_minimal()

save(PT_bnd3.6668,file="data/PT_bnd3.6668.xdr")

# interpolate land price on grid
load("data/key_code_sf.xdr") # key_code_sf
key_code_sf.6668=st_transform(key_code_sf,6668)
plot(key_code_sf.6668$geometry,add=T,col=NA,border="blue")

p2=p+geom_sf(
  data=key_code_sf.6668,
  fill=NA,
  color="blue",
  linewidth=0.6
)

p3=p2+geom_sf(
  data=tL_price,
   color="green",
  size=0.8,
  alpha=0.8,
  inherit.aes = FALSE         # 既存の aes(fill=...) などを引き継がない
)


# interpolate land rent

library(sf)
library(terra)
library(gstat)
library(dplyr)

# --- 入力 ---
# pts_sf: POINTのsf（列: value）
# polys_sf: ポリゴンのsf
# ※ CRSを合わせる
# pts_sf   <- st_transform(pts_sf, st_crs(polys_sf))

pts_sf=tL_price %>% st_transform(6689)
polys_sf=key_code_sf.6668 %>% st_transform(6689)
#  st_area(key_code_sf.6668[1,])
# --- 1) サンプル点をSpatVectorに変換 ---
pts_sf=mutate(pts_sf,value=price)
pts_v <- vect(pts_sf)
values(pts_v)$value <- pts_sf$value

# --- 2) 予測グリッドの作成（多角形範囲を覆う格子） ---
# 解像度は解析スケールに合わせて調整
res_m <- 1000  # 1kmグリッド（例）
polys_v <- vect(polys_sf)
r_tmpl  <- rast(polys_v, resolution = res_m)
g <- as.points(r_tmpl, values=FALSE)          # 予測ピクセル中心点
g <- g[polys_v, ]                              # ポリゴン範囲でクリップ

# --- 3) 半変量関数のフィット ---
# gstatはspクラスベースのため、一時的にspに変換
pts_sp <- as(pts_sf, "Spatial")
vgm_emp <- variogram(value ~ 1, data = pts_sp)
vgm_fit <- fit.variogram(vgm_emp, vgm(c("Sph", "Exp", "Gau", "Mat")))

# --- 4) Ordinary Kriging ---
g_sp <- as(st_as_sf(g), "Spatial")
ok <- gstat::krige(value ~ 1, locations = pts_sp, newdata = g_sp, model = vgm_fit)

# --- 5) 予測をラスタへ書き戻し ---
pred_sf <- st_as_sf(ok)
pred_r  <- rasterize(vect(pred_sf), r_tmpl, field = "var1.pred")

# --- 6) ゾーン統計（ポリゴンごとの平均/合計など） ---
# 例：平均値
mean_by_poly <- terra::extract(pred_r, polys_v, fun = mean, na.rm = TRUE) |>
  as.data.frame() |>
  rename(mean_value = last) |>
  mutate(ID = polys_v$KEY_CODE)  # ポリゴンID列名は実データに合わせる

# 例：合計（値×ピクセル面積の合計を取りたい場合）
# ピクセル値は“密度”とみなし、面積で重み付け → 合計
cell_area <- prod(res(pred_r)) # 地図の単位に依存（投影座標推奨）
sum_by_poly <- terra::extract(pred_r, polys_v, fun = function(x) sum(x, na.rm=TRUE)*cell_area)


polys_sf
dim(mean_by_poly)

library(ggplot2)
p=ggplot(polys_sf) +
  # geom_sf(aes(fill = (mean_by_poly$mean_value)), color = NA) +
  geom_sf(aes(fill = log(mean_by_poly$mean_value)), color = NA) +
  scale_fill_viridis_c(option = "C", direction = 1, name = "指標（対数）") +
  labs(title = "price") +
  theme_minimal()

save(mean_by_poly,file="data/LP_mean_2025.xdr")

#####
