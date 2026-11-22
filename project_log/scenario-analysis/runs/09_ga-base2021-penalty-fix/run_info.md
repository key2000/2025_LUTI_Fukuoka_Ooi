# シナリオ分析 実行記録（自動生成）

- 実行日時: 2026-11-18 15:35
- active_calibration: `09_ga-base2021-penalty-fix`（project_log/calibration/09_ga-base2021-penalty-fix/）
- git commit: bec2f2e（scripts/ に未コミットの変更あり）

## パラメータ

- alpha_a = 0.259841
- gamma_0 = 0.51256
- gamma_1 = 0.582031
- theta_H = 0.217296
- theta_L = 2.97454
- k_i: n = 847, min = 0.05587, median = 0.202, max = 0.2852

## シナリオ収束

- 道路配分 max_gap = 0.001、LUTIループ 混合比 1/(n+1)・40回固定
- scn0（Base Scenario (Current Conditions)）: iterations = 40, final_diff = 16.2, final_diff_avg = 0.00962, 総雇用 = 1330482
- scn1（Scenario 1: Uniform Distribution）: iterations = 40, final_diff = 40.4, final_diff_avg = 0.0241, 総雇用 = 1330482
- scn2（Scenario 2: CBD-Distance Weighted）: iterations = 40, final_diff = 10.9, final_diff_avg = 0.00646, 総雇用 = 1330483
