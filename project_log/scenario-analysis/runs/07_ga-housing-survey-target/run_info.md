# シナリオ分析 実行記録（自動生成）

- 実行日時: 2026-10-28 12:27
- active_calibration: `07_ga-housing-survey-target`（project_log/calibration/07_ga-housing-survey-target/）
- git commit: f3ed6d9（scripts/ に未コミットの変更あり）

## パラメータ

- alpha_a = 0.284488
- gamma_0 = 0.496493
- gamma_1 = 0.575761
- theta_H = 0.251931
- theta_L = 3.3893
- k_i: n = 847, min = 0.07387, median = 0.202, max = 0.3102

## シナリオ収束

- 道路配分 max_gap = 0.001、LUTIループ msa = 0.5, tol = 100
- scn0（Base Scenario (Current Conditions)）: converged = TRUE, iterations = 1, final_diff = 44.6, 総雇用 = 1226208
- scn1（Scenario 1: Uniform Distribution）: converged = TRUE, iterations = 3, final_diff = 82.9, 総雇用 = 1226206
- scn2（Scenario 2: CBD-Distance Weighted）: converged = TRUE, iterations = 5, final_diff = 53.4, 総雇用 = 1226209
