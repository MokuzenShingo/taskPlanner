## taskPlanner 概要
タスク管理ソフト
* 日々のタスク情報を蓄積・表示しモチベーションを保つ
* 未消化タスクの傾向、タスク時間見積もりの傾向を分析し、タスク設定能力を向上させる

デモ： https://mokuzenshingo.shinyapps.io/taskPlanner/

### Dashboard
対象日（デフォルトは当日、Submitで設定可能）のタスク進捗状況を表示します。
* タスクポイント：各タスクの重要度と緊急度の積を合計した値。
* タスク消化率：予定タスク数を完了タスク数で除した値。
* タスク時間：予定タスク時間を実績タスク時間で除した値。
* タスクネットワーク：タスク間の依存関係。進捗（未完了、完了、破棄）で色が変化する。

### Submit
タスク情報の編集画面。
* タスク情報の入力、修正
* タスク完了報告の登録

### Advisor
タスクの消化状況を集計し、タスクの消化傾向、タスク時間の見積傾向を分析します。
* タスクポイント、タスク消化状況、タスク時間の推移
* 消化・未消化タスクの傾向を分析・表示
* タスク時間の予実傾向を分析・表示

### Tasks
タスクテーブルの一覧を表示します。
* 全タスクの一覧表示
* タスクの検索

### Utility
各種設定・初期化
* グループ（タスクのカテゴリ）の編集
* タスクテーブルの初期化
* タスクテーブルをCVSファイルで保存

## インストール

### Rのインストール
https://www.r-project.org

### RStudioのインストール
https://www.rstudio.com

### ファイルのダウンロード
https://github.com/MokuzenShingo/taskPlanner/archive/master.zip

### 必要パッケージのインストール

```{r}
install.packages("shiny", dependencies = T)
install.packages("shinydashboard", dependencies = T)
install.packages("dplyr", dependencies = T)
install.packages("visNetwork", dependencies = T)
install.packages("rhandsontable", dependencies = T)
install.packages("DT", dependencies = T)
install.packages("xts", dependencies = T)
install.packages("rpart", dependencies = T)
install.packages("plotly", dependencies = T)
install.packages("stringr", dependencies = T)
```
エラーが出る場合は、R本体を最新のものにしてください。

## 初期設定
shinyアプリを立ち上げます。  
app.R　をRStudioで開いて「Run App」(左上のパネルの右上)ボタンを押します。

### グループ（タスクのカテゴリ）設定
サイドバーから「Utility」画面を開きます。 
グループ1とグループ2のテキストボックスにタスクのカテゴリを入力してください。  
半角カンマ区切りで複数登録可能です。   

(例)
```
設計,コーディング,ドキュメント作成,未分類
```
「グループ設定の保存」ボタンを押すとグループが登録されます。  
（dataフォルダにgroupList.RDataとしてグループ情報が保存されます。）

### タスクテーブルの初期化
「タスクテーブルの初期化」ボタンを押すことでタスクテーブル（すべてのタスク情報）が初期化されます。  
（dataフォルダにtaskTable.RDataとして保存されます。）

## 基本操作
初期設定を除き、ユーザが操作するのはSubmit画面が主になります。  
画面下の日付で、編集したいタスク情報の日付を選択します。

### タスク情報の入力・修正
Submit画面中央テーブルでタスク情報を入力してください。
画面下、「保存ボタン」を押すことでタスク情報が保存されます。
1. name欄にタスク名を入力する
2. to欄に該当タスクの目的となるタスクIDを入力する
3. planTime欄に予定所用時間（単位：h）を入力する
4. その他group、importance(重要度)、urgency(緊急度）を入力する
5. 「保存ボタン」を押し、タスク編集内容を保存する。

### タスク情報の削除
ID欄を空白にして、保存するとそのタスク情報は削除されます。

### タスク完了報告

1. 画面下、ID欄に該当タスクのIDを入力する
2. 画面下、実時間欄に実際にタスクにかかった時間を入力する
3. 画面下、コメント欄にメモを残す（任意）
4. 「保存ボタン」を押し、タスク編集内容を保存する

（実行する必要がなくなったタスクなど）
実時間欄に0を入力、保存することで該当タスクが「破棄」されます。
タスク情報を削除せずに、集計から外されます。
