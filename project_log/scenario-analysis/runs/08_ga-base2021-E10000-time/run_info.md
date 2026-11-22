# シナリオ分析 実行記録（自動生成）

- 実行日時: 2026-11-10 07:03
- active_calibration: `08_ga-base2021-E10000-time`（project_log/calibration/08_ga-base2021-E10000-time/）
- git commit: bec2f2e（scripts/ に未コミットの変更あり）

## パラメータ

- alpha_a = 0.29553
- gamma_0 = 0.474842
- gamma_1 = 0.568747
- theta_H = 0.23095
- theta_L = 2.88641
- k_i: n = 847, min = 0.1037, median = 0.204, max = 0.2942

## シナリオ収束

- 道路配分 max_gap = 0.001、LUTIループ 混合比 1/(n+1)・40回固定
- scn0（Base Scenario (Current Conditions)）: iterations = 40, final_diff = 19.6, final_diff_avg = 0.0117, 総雇用 = 1330482
- scn1（Scenario 1: Uniform Distribution）: iterations = 40, final_diff = 28, final_diff_avg = 0.0166, 総雇用 = 1330482
- scn2（Scenario 2: CBD-Distance Weighted）: iterations = 40, final_diff = 15.6, final_diff_avg = 0.00925, 総雇用 = 1330483
