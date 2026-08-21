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

## この内容がどの較正状態に基づくか

現時点（2026-08-20）は **k_i を固定値0.1** で実行した結果（スクリプト2045行目
`k_i <- rep(0.1, nz_res) # 260807方針: GA較正のk_i(best_k_i)は使わず固定値0.1とする`）。
`project_log/calibration/` のいずれの較正結果（k_iの逆算・緩和・クリギング等）も
現時点では反映されていない。

**再生成してこのファイルを上書きするたびに、この節を実際に使ったk_iの状態に書き換えること。**
