# plots/

シナリオ別（現状 / シナリオB: 雇用一律再配置 / シナリオC: CBD距離加重再配置）の
リンク別交通量（太さ）・速度（色）マップ。較正の複数手法を比較するログではないため、
`project_log/calibration/` のようなNNフォルダ運用はせず、固定ファイル名で上書きしていく。
過去版が見たい場合はGit履歴（`git log -- project_log/scenario-analysis/plots/`）を辿る。

- `scenario_flow_speed_maps.png` — 3シナリオ（現状/B/C）を横並びにした比較図（太さ=交通量、色=速度）
- `scn0_flow_speed_map.png` / `scn1_flow_speed_map.png` / `scn2_flow_speed_map.png` — シナリオ別単体図
- `scenario_flow_diff_maps.png` — シナリオB・Cの現状からの交通量差分（太さ=変化量の絶対値、色=増減）を横並びにした比較図
- `scenario_speed_diff_maps.png` — シナリオB・Cの現状からの速度差分（太さ=変化量の絶対値、色=増減）を横並びにした比較図
- `scn1_vs_scn0_flow_diff_map.png` / `scn2_vs_scn0_flow_diff_map.png` — シナリオ別 交通量差分の単体図
- `scn1_vs_scn0_speed_diff_map.png` / `scn2_vs_scn0_speed_diff_map.png` — シナリオ別 速度差分の単体図

`scripts/Fukuoka_LUTI_model_260617.R` の `#シナリオ別 交通量・速度マップ####` および
`#シナリオ別 交通量・速度 差分マップ（シナリオB/C − 現状）####` セクションで生成される
（`project_log/scenario-analysis/plots/` に `ggsave()` で保存）。差分マップは色が発散配色（青=減少、赤=増加）、
太さが変化量の絶対値（大きく変化したリンクほど太い）。

## 交通量の単位（pcu/日）について

pcu = Passenger Car Unit（乗用車換算台数）。車種ごとの道路容量への影響度を乗用車換算した単位で、
道路交通工学で容量を表す標準的な単位。`link10$cap`（道路種別ごとの容量、`scripts/Fukuoka_LUTI_model_260617.R`
のcap.df）はpcu/hour表記（国総研資料に基づく）だが、`capacity = link10$cap/KK`（`KK=0.1`、いわゆるK値＝
ピーク時/日交通量の比率）により日単位相当に換算して交通配分している。需要側（`l_i_j × P.car`）も
「1世帯1日1往復通勤」想定の日単位の量のため、`flow`（このplots/の交通量マップが示す値）は
pcu/hourではなく**pcu/日相当**。

## 鉄道リンク別利用者数マップ

- `scenario_rail_flow_maps.png` — 3シナリオを横並びにした鉄道リンク別利用世帯数マップ（太さ・色とも利用世帯数/日）
- `scn0_rail_flow_map.png` / `scn1_rail_flow_map.png` / `scn2_rail_flow_map.png` — シナリオ別単体図
- `scenario_rail_flow_diff_maps.png` — シナリオB・Cの現状からの鉄道利用世帯数差分を横並びにした比較図
- `scn1_vs_scn0_rail_flow_diff_map.png` / `scn2_vs_scn0_rail_flow_diff_map.png` — シナリオ別 差分単体図
- `scenario_rail_flow_maps_mesh.png` / `scn0_rail_flow_map_mesh.png` 等 — 上記の1kmメッシュ背景付き版
- `scenario_rail_flow_diff_maps_mesh.png` / `scn1_vs_scn0_rail_flow_diff_map_mesh.png` 等 — 差分マップの1kmメッシュ背景付き版

`scripts/Fukuoka_LUTI_model_260617.R` の `#鉄道 リンク別利用者数マップ####`、
`#鉄道 リンク別利用者数 差分マップ（シナリオB/C − 現状）####`、および
`#鉄道 リンク別利用者数マップ・差分マップ（1kmメッシュ背景・試作）####` セクションで生成される。
1kmメッシュ背景版は、道路の「主要道路のみ・1kmメッシュ背景」版と同じパターンで
`key_code_sf`の境界線（`gray60`）を背景に重ねたもの。

道路と異なり鉄道配分は混雑関数を持たない（BPR型の速度低下を考慮しない固定容量=10^5の
プレースホルダ）ため、「太さ=交通量・色=速度」という道路と同じ組合せの地図は作れない。
速度に相当する情報がないため、太さ・色とも「利用世帯数（`l_i_j × P.rail` の配分結果、
**世帯/日**。道路のpcu/日とは単位が異なる）」の単一指標のみを地図化している。

鉄道網データはメッシュ重心と最寄駅・駅と線形頂点を結ぶ合成リンク（`type=="access"`）を含むが、
これは実在の軌道ではないため地図描画からは除外している（`type %in% c("rail","subway")` で
フィルタ）。

配分結果（`rail_link_flow`）はリンクごとに方向別（ID1→ID2 と ID2→ID1）の値を持つが、
物理的な同一区間として1本の線で描画するため、往復方向の利用世帯数を合計した値
（`flow = flow_fwd + flow_rev`）を使っている点が道路の交通量マップ（方向別にそのまま描画）と異なる。

鉄道網データ自体は `scripts/Fukuoka_OSM_03_copied.R` の「rail network」節（OSM抽出→駅接続→
メッシュ重心接続）で構築し、`data/osm/rail_network.xdr` / `data/osm/rail_network.nodes.xdr` に
保存している。`data/` はGit管理外のため、このリポジトリをcloneしただけでは存在せず、
上記スクリプトの再実行が必要。

### 差分マップが博多駅周辺で支配的になる点について

`scenario_rail_flow_diff_maps.png` は差分の絶対値最大でスケールを揃えているため
（CLAUDE.md記載の`sym_limits()`方針）、博多駅周辺（複数路線が収束するハブ）のリンクで
シナリオ間の差分が突出して大きく、それ以外の区間はほぼ白（変化なし）に見える。
これはコードの不具合ではなく、放射状の鉄道網では末端の変化が積算されてハブ付近の
リンクに集中するという実際の構造を反映したもの（絶対値マップでも同じ場所が最大流動量）。
末端側の変化を見たい場合は、現状は個別にデータを絞り込んで確認する必要がある
（道路の「主要道路のみ」版のような専用フィルタ版は今のところ未整備）。

また、鉄道網トポロジー構築（`scripts/Fukuoka_OSM_03_copied.R`の駅接続・線形分割処理）は
上下線が別ジオメトリのOSMデータに由来し、簡略化後も同一ノードペア（ID1, ID2）に
複数の物理セグメントが対応するケースが少数残っている（2600リンク中42ペア、約1.6%）。
このケースでは`rail_network`側で同じ場所に同じflow値の線が重なって描画されるため
見た目には影響しないが、リンク数を数える集計をする場合は注意。

## この内容がどの較正状態に基づくか

現時点（2026-08-24）は **k_i を固定値0.1** で実行した結果（`k_i <- rep(0.1, nz_res) # 260807方針:
GA較正のk_i(best_k_i)は使わず固定値0.1とする`）。`project_log/calibration/` のいずれの較正結果
（k_iの逆算・緩和・クリギング等）も現時点では反映されていない。

**再生成してこのファイルを上書きするたびに、この節を実際に使ったk_iの状態に書き換えること。**
