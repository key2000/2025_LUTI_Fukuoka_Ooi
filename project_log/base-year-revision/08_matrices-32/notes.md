# 08 中心地を増やした所要時間行列（2026-11-08）

## 今の行列の再現（`reproduce_17.R`）
- 道路（混雑なし）`data/dists0.xdr`（884×18）：link10 を `FFtime = linklen/maxspeed*60/1000`（分）で `get_distance_matrix(mch)`。**全セル一致、NAの位置も一致**。
- 鉄道 `data/railway_dist.xdr`（883×18）：rail_network を往復の向きに複製し、rail 60・subway 40・access 5 km/h。**全セル一致**。
- 自転車 `data/dists0.bike.xdr`：02 の `bike_check.R` で一致を確認済み（link10 を 15km/h）。
- → 3つとも、中心地を変えて同じ方法で作り直せる。

## 新しい行列（`build_matrices.R`）
出力：`data/processed/centers_E<E>/`（Git管理外）に `dists0.xdr`・`railway_dist.xdr`・`dists0.bike.xdr`・`work_zone.xdr`（中心地の KEY_CODE、`work_zone_new`）。

| E | 中心地（ファイル上） | メインで孤立の 50302329 を除いた後 |
|---|--:|--:|
| 2万 | 25 | 24 |
| 1万 | 33 | **32** |
| 5千 | 47 | 46 |

- 追加の中心地はモデルの対象範囲内のメッシュに限定（経済センサスの表は福岡県全体なので）。
- 道路・自転車のNAは、今と同じ36の孤立した居住メッシュの行だけ（新しい中心地の列に余分なNAはない）。鉄道はNAなし。
- まだメインスクリプトには読み込ませていない。メインは `data/dists0.xdr` などを固定のパスで読むので、
  中心地のセットを切り替えられるようにする変更が必要（t25 で実施し、記録する）。
