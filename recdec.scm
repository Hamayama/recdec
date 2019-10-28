;; -*- coding: utf-8 -*-
;;
;; recdec.scm
;; 2019-10-28 v1.01
;;
;; ＜内容＞
;;   Gauche で、有理数と循環小数の相互変換を行うためのモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/recdec
;;
(define-module recdec
  (use srfi-13) ; string-for-each,string-trim-both用
  (export
    real->recdec
    recdec->real))
(select-module recdec)

;; 循環部分の範囲を示す記号
(define *rec-start-char* #\{) ; 始点
(define *rec-end-char*   #\}) ; 終点

;; 有理数を循環小数の文字列に変換する
;;   ・循環部分の範囲は記号で囲う
;;       例. 0.123454545 ... ==> "0.123{45}"
(define (real->recdec num)
  ;; 引数のチェック
  (unless (number? num)
    (error "number required, but got:" num))
  (unless (= (imag-part num) 0)
    (error "complex number with non-zero imaginary part is not supported:" num))
  (when (infinite? num)
    (error "infinite number is not supported:" num))
  (when (nan? num)
    (error "nan is not supported:" num))
  ;; 正確数に変換
  (set! num (exact num))
  ;; 文字列の出力
  (with-output-to-string
    (lambda ()
      (let* ((minus (< num 0))              ; マイナス符号フラグ
             (num1  (if minus (- num) num)) ; 符号をプラスにする
             (n     (numerator   num1))     ; 有理数の分子
             (d     (denominator num1))     ; 有理数の分母
             (q     (quotient  n d))        ; 商
             (r     (remainder n d))        ; 余り
             (i     0))                     ; 小数部の位置
        ;; 整数部の出力
        (if minus (display #\-))
        (display q)
        ;; 小数のとき
        (unless (= r 0)
          ;; 小数点の出力
          (display #\.)
          ;; 循環部分の範囲を求める
          (receive (start end)
              (%get-rec-range r (lambda (r) (remainder (* r 10) d)))
            ;; 小数部の各桁を求めて出力
            (let loop ()
              (set! n (* r 10))
              (set! q (quotient  n d))
              (set! r (remainder n d))
              (when (= i start) (display *rec-start-char*))
              (display (integer->digit q))
              (cond
               ((= r 0))
               ((= i end) (display *rec-end-char*))
               (else      (inc! i) (loop))))))))))


;; 循環部分の範囲を求める(フロイドの循環検出法)(内部処理用)
;;   r     数列の初期値
;;   calc  数列の計算式
(define (%get-rec-range r calc)
  ;; フロイドの循環検出法
  (let ((fast  (calc (calc r))) ; 高速移動の計算結果
        (slow  (calc r))        ; 低速移動の計算結果
        (start 0)               ; 循環部分の始点
        (end   0))              ; 循環部分の終点
    ;; 循環部分を検出する
    (until (= fast slow)
      (set! fast (calc (calc fast)))
      (set! slow (calc slow)))
    ;; 循環部分の始点を求める
    (set! fast r)
    (until (= fast slow)
      (set! fast (calc fast))
      (set! slow (calc slow))
      (inc! start))
    ;; 循環部分の終点を求める
    (set! end start)
    (set! fast (calc fast))
    (until (= fast slow)
      (set! fast (calc fast))
      (inc! end))
    ;; 戻り値を多値で返す
    (values start end)))


;; 循環小数の文字列を有理数に変換する
;;   ・循環部分の範囲は記号で囲われていること
;;       例. "0.123{45}"
(define (recdec->real num-st)
  ;; 引数のチェック
  (unless (string? num-st)
    (error "string required, but got:" num-st))
  ;; 前後の空白の削除
  (set! num-st (string-trim-both num-st))
  ;; 循環小数の文字列の分解
  (receive (split-ok sign-st int-st frac-st rec-st)
      (%split-recdec-str num-st)
    ;; 分解できなかったとき
    (unless split-ok
      (error "couldn't convert:" num-st))
    ;; 有理数に変換
    (let ((int-num  (x->integer int-st))     ; 整数部
          (frac-num (x->integer frac-st))    ; 小数部
          (rec-num  (x->integer rec-st))     ; 循環小数部
          (frac-len (string-length frac-st)) ; 小数部の桁数
          (rec-len  (string-length rec-st))  ; 循環小数部の桁数
          (rec-sum  0)                       ; 循環小数部の計算値
          (ret      0))                      ; 結果
      ;; 無限等比級数の和を計算
      (unless (or (= rec-num 0) (= rec-len 0))
        (set! rec-sum (/ rec-num (- (expt 10 rec-len) 1))))
      ;; 全体を計算 (整数部 + 小数部 + 循環小数部)
      (set! ret (+ int-num
                   (* (expt 10 (- frac-len))
                      (+ frac-num rec-sum))))
      (if (equal? sign-st "-") (- ret) ret))))


;; 循環小数の文字列の分解(内部処理用)
;;   ・符号部、整数部、小数部、循環小数部の文字列に分解する
;;   ・循環部分の範囲は記号で囲われていること
;;       例. "0.123{45}"
(define (%split-recdec-str num-st)
  (let ((num-len    (string-length num-st)) ; 数値文字列の長さ
        (sign-flag  #f) ; 符号の有無
        (zero-flag  #f) ; 先頭のゼロの有無
        (int-index  #f) ; 整数部の開始位置
        (frac-index #f) ; 小数部の開始位置
        (rec-index  #f) ; 循環小数部の開始位置
        (end-index  #f) ; 循環小数部の終了位置
        (sign-st    #f) ; 符号部の文字列
        (int-st     #f) ; 整数部の文字列
        (frac-st    #f) ; 小数部の文字列
        (rec-st     #f) ; 循環小数部の文字列
        (err-flag   #f) ; エラーフラグ
        (i          -1) ; 位置
        (mode       0)) ; 解析モード
    ;; 数値文字列の解析
    (string-for-each
     (lambda (c)
       (unless err-flag
         (inc! i)
         (let loop ()
           ;; 解析モードによって場合分け
           (case mode
             ;; 符号のチェック
             ((0)
              (case c
                ((#\+ #\-) (set! sign-flag #t) (inc! mode))
                (else  (inc! mode) (loop))))
             ;; 先頭のゼロのスキップ
             ((1)
              (case c
                ((#\0) (set! zero-flag #t))
                (else  (set! int-index i) (inc! mode) (loop))))
             ;; 整数部のチェック
             ((2)
              (case c
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                ((#\.) (set! frac-index i) (inc! mode))
                (else  (set! err-flag #t))))
             ;; 小数部のチェック
             ((3)
              (case c
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                (else
                 (if (eqv? c *rec-start-char*)
                   (begin (set! rec-index i) (inc! mode))
                   (set! err-flag #t)))))
             ;; 循環小数部のチェック
             ((4)
              (case c
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                (else
                 (if (eqv? c *rec-end-char*)
                   (begin (set! end-index i) (set! mode 10))
                   (set! err-flag #t)))))
             ;; 終端のチェック
             ((10)
              (set! err-flag #t))
             ))
         ))
     num-st)
    ;; 符号部、整数部、小数部、循環小数部の文字列を取得
    (unless err-flag
      (set! sign-st (if sign-flag
                      (substring num-st 0 1)
                      ""))
      (set! int-st  (if int-index
                      (substring num-st int-index (or frac-index num-len))
                      ""))
      (if (and zero-flag (equal? int-st ""))
        (set! int-st "0"))
      (set! frac-st (if frac-index
                      (substring num-st (+ frac-index 1) (or rec-index num-len))
                      ""))
      (set! rec-st  (if rec-index
                      (substring num-st (+ rec-index  1) (or end-index num-len))
                      ""))
      ;; エラーチェック
      ;;   ・整数部と小数部と循環小数部が全て空のときはエラー
      ;;   ・循環小数部が不完全のときはエラー
      (if (and (equal? int-st "") (equal? frac-st "") (equal? rec-st ""))
        (set! err-flag #t))
      (if (and rec-index (not end-index))
        (set! err-flag #t))
      )
    ;; 戻り値を多値で返す(先頭は成功フラグ)
    (if err-flag
      (values #f #f #f #f #f)
      (values #t sign-st int-st frac-st rec-st))))


