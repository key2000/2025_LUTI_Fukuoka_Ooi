# plots/

シナリオ別（現状 / シナリオB: 雇用一律再配置 / シナリオC: CBD距離加重再配置）の
リンク別交通量（太さ）・速度（色）マップ。較正の複数手法を比較するログではないため、
`project_log/calibration/` のようなNNフォルダ運用はせず、固定ファイル名で上書きしていく。
過去版が見たい場合はGit履歴（`git log -- project_log/scenario-analysis/plots/`）を辿る。

- `scenario_flow_speed_maps.png` — 3シナリオを横並びにした比較図
- `scn0_flow_speed_map.png` / `scn1_flow_speed_map.png` / `scn2_flow_speed_map.png` — シナリオ別単体図

`scripts/Fukuoka_LUTI_model_260617.R` の `#シナリオ別 交通量・速度マップ####` セクションで生成される
（`project_log/scenario-analysis/plots/` に `ggsave()` で保存）。

## この内容がどの較正状態に基づくか

現時点（2026-08-20）は **k_i を固定値0.1** で実行した結果（スクリプト2045行目
`k_i <- rep(0.1, nz_res) # 260807方針: GA較正のk_i(best_k_i)は使わず固定値0.1とする`）。
`project_log/calibration/` のいずれの較正結果（k_iの逆算・緩和・クリギング等）も
現時点では反映されていない。

**再生成してこのファイルを上書きするたびに、この節を実際に使ったk_iの状態に書き換えること。**
