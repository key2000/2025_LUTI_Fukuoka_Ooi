# シナリオ分析 実行記録（自動生成）

- 実行日時: 2026-11-08 21:10
- active_calibration: `07_ga-housing-survey-target`（project_log/calibration/07_ga-housing-survey-target/）
- git commit: bec2f2e（scripts/ に未コミットの変更あり）

## パラメータ

- alpha_a = 0.284488
- gamma_0 = 0.496493
- gamma_1 = 0.575761
- theta_H = 0.251931
- theta_L = 3.3893
- k_i: n = 847, min = 0.07387, median = 0.202, max = 0.3102

## シナリオ収束

- 道路配分 max_gap = 0.001、LUTIループ 混合比 1/(n+1)・40回固定
- scn0（Base Scenario (Current Conditions)）: iterations = 40, final_diff = 13.1, final_diff_avg = 0.00778, 総雇用 = 1226208
- scn1（Scenario 1: Uniform Distribution）: iterations = 40, final_diff = 47.3, final_diff_avg = 0.0281, 総雇用 = 1226206
- scn2（Scenario 2: CBD-Distance Weighted）: iterations = 40, final_diff = 34.7, final_diff_avg = 0.0207, 総雇用 = 1226209
