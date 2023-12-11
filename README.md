# 問題
英小文字からなるサイズNの文字列Sが与えられる。
AとBが空文字で初期化されている文字列Tに対して次のルールで操作を行う。
i = 1,2,...Nの順で
- iが奇数の時にAのターンで、Tをそのままにするか、Sのi文字目をTの末尾に追加する。
- iが偶数の時にBのターンで、Tをそのままにするか、Sのi文字目の末尾に追加する。
Aは最終的なTを辞書順最小にしようとし、Bさんは最終的なTを辞書順最大にしようとする。両者が最適な行動をとった場合の最終的な文字列Tを求めてください。なお、両者はお互いに相手の行動方針は知っているものとし、文字列Sもすべて把握しているとする。

## 入力
N
S

## 出力
T

## アルゴリズムの解説
- Nが偶数の時は一番最後の文字をTに追加し、N-1が偶数なら後ろから2番目の文字をTに追加する。
- i番目の文字が偶数番目なら、Tの文字列の先頭と比較して辞書順が以上なら追加する。i番目の文字が奇数番目ならTの文字列の先頭と比較して辞書順が未満なら追加する。これをSの先頭まで繰り返す。