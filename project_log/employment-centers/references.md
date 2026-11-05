# 文献リスト：従業中心地（雇用中心）の選定

2026-10-30 作成。「確認済み」は Web 検索で書誌情報を確認したもの。「要確認」は Claude の知識によるもので、
引用前に CiNii / Google Scholar で号・ページを確かめること。

## 1. 雇用中心（employment subcenter）の抽出手法

研究の流れは大きく3つ：①閾値法、②密度関数を使う統計的方法、③局所空間自己相関（LISA）。
Knaap (2024) は、雇用中心の定義・抽出方法について研究者の間で大きな意見の不一致があると整理している。

| 文献 | 内容 | 確認 |
|---|---|---|
| Giuliano, G. & Small, K. A. (1991) "Subcenters in the Los Angeles region." *Regional Science and Urban Economics*, 21(2), 163–182. [PDF](https://sites.socsci.uci.edu/~ksmall/SUBCEN1.pdf) | 閾値法の出発点。密度と総従業者数の絶対的な閾値で雇用中心を定義 | 確認済み |
| McMillen, D. P. (2001) "Nonparametric employment subcenter identification." *Journal of Urban Economics*, 50(3), 448–473. | 都心からの距離の密度関数を局所回帰で推定し、有意に高い場所を中心とする | 要確認 |
| Riguelle, F., Thomas, I. & Verhetsel, A. (2007) "Measuring urban polycentrism: a European case study and its implications." *Journal of Economic Geography*, 7(2), 193–215. doi:10.1093/jeg/lbl025 [IDEAS](https://ideas.repec.org/a/oup/jecgeo/v7y2007i2p193-215.html) | ベルギー。局所自己相関（Local Moran's I）が雇用のまとまりの検出に特に適していると結論 | 確認済み |
| Knaap 他 (2024) "Measuring Two Decades of Urban Spatial Structure: The Evolution of Agglomeration Economies in American Metros." [PDF](https://cogs.sdsu.edu/publication/knaap-2024-measuring-two/knaap-2024-measuring-two.pdf) | 雇用中心の定義・抽出方法の整理と、新しい抽出手法の提案 | 確認済み |
| ロサンゼルスでの複数の抽出手法の比較 [PDF](https://la-plan.caset.buffalo.edu/papers/three_procedures.pdf) | 閾値法・統計的方法などの比較 | 確認済み（著者・書誌は本文で確認） |

## 2. 手法の原論文

| 文献 | 内容 | 確認 |
|---|---|---|
| Getis, A. & Ord, J. K. (1992) "The analysis of spatial association by use of distance statistics." *Geographical Analysis*, 24(3), 189–206. | Gi 統計量の提案 | 要確認 |
| Ord, J. K. & Getis, A. (1995) "Local spatial autocorrelation statistics: distributional issues and an application." *Geographical Analysis*, 27(4), 286–306. | Gi\* の分布と検定 | 要確認 |
| Anselin, L. (1995) "Local indicators of spatial association—LISA." *Geographical Analysis*, 27(2), 93–115. | Local Moran's I（LISA）の提案 | 要確認 |

## 3. マニュアル・ソフトウェア

- ArcGIS Pro「Hot Spot Analysis (Getis-Ord Gi\*)」：[ツールの説明](https://pro.arcgis.com/en/pro-app/3.6/tool-reference/spatial-statistics/hot-spot-analysis.htm)（同サイトに計算の仕組みの解説ページあり）
- R `spdep` パッケージ：`localmoran()` / `localmoran_perm()`（Local Moran's I）、`localG()` / `localG_perm()`（Gi\*）のヘルプ
- [Getis–Ord statistics（Wikipedia）](https://en.wikipedia.org/wiki/Getis%E2%80%93Ord_statistics)

## 4. 日本の研究

日本では、1kmメッシュの従業者密度に閾値を設ける方法や、昼間人口比率で中心性を測る方法が多い。

| 文献 | 内容 | 確認 |
|---|---|---|
| 菊池 (2008)「日本の都市地域における従業者密度分布の変容」『地理学評論』81(4) [CiNii](https://cir.nii.ac.jp/crid/1390282680181110016) | 地域メッシュ統計で157都市地域の従業者密度分布の変化（1981〜2001）を分析 | 確認済み（題目は英題から訳。原題は要確認） |
| GISA 2013 C-2-1 [PDF](https://www.gisa-japan.org/content/files/conferences/proceedings/2013cd/papers/C-2-1.pdf) | メッシュ統計で都市の拠点を抽出する手法とツール。抽出過程の恣意性を課題に挙げる | 確認済み（著者は本文で確認） |
| GISA 2008 3D-4 [PDF](https://www.gisa-japan.org/content/files/conferences/proceedings/2008cd/papers/3D-4.pdf) | ストックホルム都市圏で都心とサブセンターを識別する基準の検討 | 確認済み（著者は本文で確認） |
| ICU「メッシュ・データによる東京大都市地域構造の分析」 [PDF](https://icu.repo.nii.ac.jp/record/3012/files/%E3%83%A1%E3%83%83%E3%82%B7%E3%83%A5%E3%83%BB%E3%83%87%E3%83%BC%E3%82%BF%E3%81%AB%E3%82%88%E3%82%8B%E6%9D%B1%E4%BA%AC%E5%A4%A7%E9%83%BD%E5%B8%82%E5%9C%B0%E5%9F%9F%E6%A7%8B%E9%80%A0%E3%81%AE%E5%88%86%E6%9E%90.pdf) | 昼間人口比率の順位分布で中心地の階層を区切る | 確認済み（著者・年は本文で確認） |
| 金本良嗣・徳岡一幸 (2002)「日本の都市圏設定基準」『応用地域学研究』7, 1–15. | 都市雇用圏の定義。このモデルの対象範囲（UEA）の根拠 | 要確認 |

## 5. 集約・空間単位の問題（都心の吸収問題に関連、2026-10-30 追加）

既往の雇用中心の抽出研究は都市構造の記述が目的で、中心は「隣り合う単位のまとまり（面）」として定義されるため、
まとまり内の雇用の合算は定義どおり。中心を1メッシュ（点）で代表させて通勤先にするこのモデル特有の「吸収」の問題を
直接扱った研究は、今回の検索では見つからなかった。近い議論は次の2つ。

| 文献 | 内容 | 確認 |
|---|---|---|
| Martínez, L. M., Viegas, J. M. & Silva, E. A. (2009) "A traffic analysis zone definition: a new methodology and algorithm." *Transportation*, 36, 581–599. doi:10.1007/s11116-009-9214-z [Springer](https://springerlink.bibliotecabuap.elogim.com/doi/10.1007/s11116-009-9214-z) | 交通ゾーンの区切り方。集約による誤差・ゾーン内々の移動・統計的精度の兼ね合いを、MAUPとして扱う。案①（需要が密な都心だけ細かく）の根拠に使える | 確認済み（抜粋のみ） |
| "Wandering in cities: a statistical physics approach to urban theory"（arXiv 1511.08236）[PDF](https://arxiv.org/pdf/1511.08236) | レビュー。雇用中心の数が空間単位の大きさに依存すること（MAUP）、閾値の選び方に先験的な根拠がないことを指摘 | 確認済み（著者は本文で確認） |
| Riguelle 他 (2007)（上の1.） | 複数の集計レベルで分析（要旨より。本文未確認） | 一部確認 |

MAUP（Modifiable Areal Unit Problem、可変単位地区問題）：同じデータでも区切り方や単位の大きさで分析結果が変わる問題。
