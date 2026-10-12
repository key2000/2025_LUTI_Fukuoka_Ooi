---
order: 1
date: "2026-05-27"
title: "5変数のみのGA較正（ベースライン）"
verdict: adopted
paper: none
plot: plots/ga_convergence.png
---

## 手法

alpha_a, gamma_0, gamma_1, theta_H, theta_L の5つの構造パラメータをGAで較正。
k_i はモデル本来の定数0.1に固定したまま動かさない。

## パラメータ

- popSize: 50
- maxiter: 100
- 探索変数数: 5

## 指標

- fitness (-Loss): -0.394241
- alpha_a: 0.2827
- gamma_0: 0.2658
- gamma_1: 0.5828
- theta_H: 0.0957
- theta_L: 2.2811

## メモ

ベースラインとして採用。次はk_iも較正対象に含めることを試す（→02_ga-800dim）。

## 計算結果のキャッシュ

`ga_result.RData`（このフォルダ内、.gitignore対象・ローカルのみ）に `ga` オブジェクト一式を保存済み。
再計算せずに読み込みたい場合は `load("project_log/calibration/01_baseline-5param-ga/ga_result.RData")` で復元可能。
上記のパラメータ（popSize/maxiter/探索変数）と同じ設定で再実行すれば再現できる。
