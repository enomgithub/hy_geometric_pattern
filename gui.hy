(import [math [sin cos radians]])
(import sys)
(import [tkinter :as tk])

; キャンバスサイズ
(setv *width* 800)
(setv *height* 600)


(defclass GeometricPattern []
  (setv *d-theta-max* 20)
  (setv *deg* 360)
  (setv *d-phi* 1)

  (defn --init-- [self &optional [center-x 0] [center-y 0]]
    "パラメータを初期化し、点のリストを取得する"
    (setv (. self center-x) center-x)
    (setv (. self center-y) center-y)

    (setv (. self r)
          (int (-> 0.8
                   (* (min (. self center-x) (. self center-y)))
                   (/ 3))))
    (setv (. self a)
          (int (-> 0.8
                   (* 2 (min (. self center-x) (. self center-y)))
                   (/ 3))))
    (setv (. self phi) 0)
    (setv (. self c) 104)

    ;; theta を変動させるときの刻み幅
    (setv (. self d-theta) 1)

    ((. self fetch-points))
    
    None)
  
  (defn dec-r [self event]
    "rを減少する"
    (setv (. self r) (dec (. self r)))
    (if (< (. self r) 1)
        (setv (. self r) 1))
    None)

  (defn inc-r [self event]
    "rを増加する。Canvas の短辺からはみ出さないように上限を指定している"
    (if (< (+ (. self r) (. self a))
           (min (. self center-x) (. self center-y)))
        (setv (. self r) (inc (. self r))))
    None)
  
  (defn dec-a [self event]
    "aを減少する"
    (setv (. self a) (dec (. self a)))
    (if (< (. self a) 1)
        (setv (. self a) 1))
    None)
  
  (defn inc-a [self event]
    "aを増加する。Canvas の短辺からはみ出さないように上限を指定している"
    (if (< (+ (. self r) (. self a))
           (min (. self center-x) (. self center-y)))
        (setv (. self a) (inc (. self a))))
    None)
  
  (defn dec-d-theta [self event]
    (setv (. self d-theta) (dec (. self d-theta)))
    (if (< (. self d-theta) 1)
        (setv (. self d-theta) 1))
    None)
  
  (defn inc-d-theta [self event]
    (setv (. self d-theta) (inc (. self d-theta)))
    (if (> (. self d-theta) (. GeometricPattern *d-theta-max*))
        (setv (. self d-theta) (. GeometricPattern *d-theta-max*)))
    None)
  
  (defn dec-c [self event]
    (setv (. self c) (dec (. self c)))
    (if (< (. self c) 0)
        (setv (. self c) (% (. self c) (. GeometricPattern *deg*))))
    None)
  
  (defn inc-c [self event]
    (setv (. self c) (inc (. self c)))
    (if (>= (. self c) (. GeometricPattern *deg*))
        (setv (. self c) (% (. self c) (. GeometricPattern *deg*))))
    None)

  (defn inc-phi [self]
    (setv (. self phi) (inc (. self phi)))
    (setv (. self phi) (% (. self phi) (. GeometricPattern *deg*)))
    None)
  
  (defn fetch-points [self]
    "点の座標を取得する"
    (setv (. self points)
          (lfor theta (range 0 (. GeometricPattern *deg*) (. self d-theta))
                (, (+ (+ (. self center-x) (* (. self r) (cos (radians theta))))
                      (* (. self a) (cos (radians (+ (. self phi) (* (. self c) theta))))))
                   (+ (+ (. self center-y) (* (. self r) (sin (radians theta))))
                      (* (. self a) (sin (radians (+ (. self phi) (* (. self c) theta)))))))))

    ;; パスを閉じるために最初の点をリストの最後に追加する
    ((. self points append) (. self points [0]))

    None))


(defclass Simulator []
  (setv *ms* 16)
  
  (defn --init-- [self width height]
    (setv (. self width) width)
    (setv (. self height) height)

    ;; ウインドウとキャンバスの初期化
    (setv (. self window) ((. tk Tk)))
    ((. self window title) :string "Sample Trig Function on Tkinter")
    ((. self window resizable) :width False :height False)
    (setv (. self canvas)
          ((. tk Canvas) (. self window)
                         :width (. self width)
                         :height (. self height)))

    (setv (. self center-x) (/ width 2))
    (setv (. self center-y) (/ height 2))

    ;; 表示する文字の座標
    (setv (. self quit-x) 20)
    (setv (. self quit-y) 30)
    (setv (. self r-x) (- width 20))
    (setv (. self r-y) (- height 120))
    (setv (. self a-x) (- width 20))
    (setv (. self a-y) (- height 90))
    (setv (. self c-x) (- width 20))
    (setv (. self c-y) (- height 60))
    (setv (. self d-theta-x) (- width 20))
    (setv (. self d-theta-y) (- height 30))

    (setv (. self gp) (GeometricPattern (. self center-x) (. self center-y)))

    ;; 背景色（黒）でキャンバスを塗りつぶす
    ((. self canvas create-rectangle) 0
                                      0
                                      (. self width)
                                      (. self height)
                                      :fill "black")
    ((. self draw))
    ((. self canvas pack))

    ;; キーバインドの設定
    ((. self window bind) "<KeyPress-q>" (. self quit))
    ((. self window bind) "<KeyPress-z>" (. self gp dec-r))
    ((. self window bind) "<KeyPress-a>" (. self gp inc-r))
    ((. self window bind) "<KeyPress-x>" (. self gp dec-a))
    ((. self window bind) "<KeyPress-s>" (. self gp inc-a))
    ((. self window bind) "<KeyPress-c>" (. self gp dec-c))
    ((. self window bind) "<KeyPress-d>" (. self gp inc-c))
    ((. self window bind) "<KeyPress-v>" (. self gp dec-d-theta))
    ((. self window bind) "<KeyPress-f>" (. self gp inc-d-theta))
    
    None)
  
  (defn quit [self event]
    ((. self window destroy))
    None)
  
  (defn draw [self]

    ;; 図形の描画
    ((. self canvas create-line) (. self gp points) :fill "white" :tag "sample")
    
    ;; 文字の描画
    ((. self canvas create-text) (. self quit-x)
                                 (. self quit-y)
                                 :text "Quit: q"
                                 :tag "sample"
                                 :font (, "Arial" 12)
                                 :fill "white"
                                 :anchor (. tk W))
    ((. self canvas create-text) (. self r-x)
                                 (. self r-y)
                                 :text ((. "r: Z < {0:03d} > A" format) (. self gp r))
                                 :tag "sample"
                                 :font (, "Arial" 12)
                                 :fill "white"
                                 :anchor (. tk E))
    ((. self canvas create-text) (. self a-x)
                                 (. self a-y)
                                 :text ((. "a: X < {0:03d} > S" format) (. self gp a))
                                 :tag "sample"
                                 :font (, "Arial" 12)
                                 :fill "white"
                                 :anchor (. tk E))
    ((. self canvas create-text) (. self c-x)
                                 (. self c-y)
                                 :text ((. "c: C < {0:03d} > D" format) (. self gp c))
                                 :tag "sample"
                                 :font (, "Arial" 12)
                                 :fill "white"
                                 :anchor (. tk E))
    ((. self canvas create-text) (. self d-theta-x)
                                 (. self d-theta-y)
                                 :text ((. "d-theta: V < {0:03d} > F" format) (. self gp d-theta))
                                 :tag "sample"
                                 :font (, "Arial" 12)
                                 :fill "white"
                                 :anchor (. tk E))
    None)
  
  (defn delete [self]
    "sampleタグをつけた要素を削除する"
    ((. self canvas delete) "sample")
    None)
  
  (defn loop [self]
    ;; 一つ前の画面をクリア
    ((. self delete))

    ;; 新しい状態を取得して描画
    ((. self gp inc-phi))
    ((. self gp fetch-points))
    ((. self draw))

    ;; 約16ms後にloopを実行
    ((. self window after) (. Simulator *ms*) (. self loop))
    
    None))


(defn main []
  (setv simulator (Simulator *width* *height*))
  ((. simulator loop))
  ((. simulator window mainloop))
  0)


(when (= --name-- "__main__")
      ((. sys exit) (main)))