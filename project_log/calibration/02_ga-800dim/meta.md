---
order: 2
date: "2026-06-26"
title: 'k_i を含む852次元GA（"800dim"）'
verdict: rejected
paper: none
plot: plots/best_k_i_hist.png
---

## 手法

k_i を0.1固定にせず、847ゾーンすべてのk_iをGAの探索変数に追加。
5変数 + 847ゾーン = 852次元の実数値GAで、探索設定はベースラインと同じ
（popSize=50, maxiter=100）。

## パラメータ

- popSize: 50
- maxiter: 100
- 探索変数数: 852

## 指標

- fitness (-Loss): -14.39047
- Loss（ベースライン比）: 約36倍

## メモ

探索変数を増やしたらLossが大幅に悪化。popSize=50では852次元を実用的な
世代数で探索しきれていないと判断。GAでゾーン別k_iを丸ごと探す方針は
ここで見送り、観測データからの解析的逆算に切り替える（→03_ki-backward-calc）。

## 計算結果のキャッシュ

`ga_result.RData`（このフォルダ内、.gitignore対象・ローカルのみ）に `ga_result_calib` / `best_x` を保存済み。
`scripts/Fukuoka_LUTI_model_260617.R` の該当if(FALSE)ブロックから
`load("project_log/calibration/02_ga-800dim/ga_result.RData")` で復元可能。
上記のパラメータ（popSize/maxiter/探索変数）と同じ設定で再実行すれば再現できる。
