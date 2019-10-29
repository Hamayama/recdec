;; -*- coding: utf-8 -*-
;;
;; recdec.scm
;; 2019-10-30 v1.04
;;
;; ＜内容＞
;;   Gauche で、有理数と循環小数の相互変換を行うためのモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/recdec
;;
(define-module recdec
  (use srfi-13) ; string-for-each,string-map,string-trim-both用
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
;;   num             数値
;;   :rdx radix      出力の基数
;;   :bc1 basechar1  出力の 0を表す文字
;;   :bc2 basechar2  出力の10を表す文字(基数が11以上のときに使用)
(define (real->recdec num
                      :key ((:rdx radix) 10)
                      ((:bc1 basechar1) #\0)
                      ((:bc2 basechar2) #\a))
  ;; 引数のチェック
  (unless (number? num)
    (error "number required, but got:" num))
  (unless (= (imag-part num) 0)
    (error "complex number with non-zero imaginary part is not supported:" num))
  (when (infinite? num)
    (error "infinite number is not supported:" num))
  (when (nan? num)
    (error "nan is not supported:" num))
  (integer->digit 1 radix basechar1 basechar2)
  ;; 正確数に変換
  (set! num (exact num))
  ;; 有理数を循環小数の文字列に変換する
  (let* ((minus (< num 0))              ; マイナス符号フラグ
         (num1  (if minus (- num) num)) ; 符号をプラスにする
         (n     (numerator   num1))     ; 有理数の分子
         (d     (denominator num1))     ; 有理数の分母
         (q     (quotient  n d))        ; 商
         (r     (remainder n d))        ; 余り
         (i     0))                     ; 小数部の位置
    ;; 文字列の出力
    (with-output-to-string
      (lambda ()
        ;; 整数部の出力
        (if minus (display #\-))
        (cond
         ((and (= radix 10) (eqv? basechar1 #\0))
          (display q))
         ((and (eqv? basechar1 #\0)
               (or (<= radix 10) (eqv? basechar2 #\a)))
          (display (number->string q radix)))
         (else
          (string-for-each
           (lambda (c) (display (integer->digit
                                 (digit->integer c radix)
                                 radix basechar1 basechar2)))
           (number->string q radix))))
        ;; 小数のとき
        (unless (= r 0)
          ;; 小数点の出力
          (display #\.)
          ;; 循環部分の範囲を求める
          (receive (start end)
              (%get-rec-range r (lambda (r) (remainder (* r radix) d)))
            ;; 小数部の各桁を求めて出力
            (let loop ()
              (set! n (* r radix))
              (set! q (quotient  n d))
              (set! r (remainder n d))
              (when (= i start) (display *rec-start-char*))
              (display (integer->digit q radix basechar1 basechar2))
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
;;   num-st                数値文字列
;;   :rdx radix            入力の基数
;;   :exr extended-range?  UnicodeのNdカテゴリの文字を受け付けるかどうか
(define (recdec->real num-st
                      :key ((:rdx radix) 10)
                      ((:exr extended-range?) #f))
  ;; 数値変換手続き(基数に対応)
  (define (to-number num-st radix extended-range?)
    (when extended-range?
      (set! num-st (string-map
                    (lambda (c) (integer->digit
                                 (digit->integer c radix #t)
                                 radix))
                    num-st)))
    (or (string->number num-st radix) 0))
  ;; 引数のチェック
  (unless (string? num-st)
    (error "string required, but got:" num-st))
  (digit->integer #\1 radix extended-range?)
  ;; 前後の空白の削除
  (set! num-st (string-trim-both num-st))
  ;; 循環小数の文字列の分解
  (receive (split-ok sign-st int-st frac-st rec-st)
      (%split-recdec-str num-st radix extended-range?)
    ;; 分解できなかったとき
    (unless split-ok
      (error "couldn't convert:" num-st))
    ;; 有理数に変換する
    (let ((int-num  (to-number int-st  radix extended-range?)) ; 整数部
          (frac-num (to-number frac-st radix extended-range?)) ; 小数部
          (rec-num  (to-number rec-st  radix extended-range?)) ; 循環小数部
          (frac-len (string-length frac-st)) ; 小数部の桁数
          (rec-len  (string-length rec-st))  ; 循環小数部の桁数
          (rec-sum  0)                       ; 循環小数部の計算値
          (ret      0))                      ; 結果
      ;; 無限等比級数の和を計算
      (unless (or (= rec-num 0) (= rec-len 0))
        (set! rec-sum (/ rec-num (- (expt radix rec-len) 1))))
      ;; 全体を計算 (整数部 + 小数部 + 循環小数部)
      (set! ret (+ int-num
                   (* (expt radix (- frac-len))
                      (+ frac-num rec-sum))))
      (if (equal? sign-st "-") (- ret) ret))))


;; 循環小数の文字列の分解(内部処理用)
;;   ・符号部、整数部、小数部、循環小数部の文字列に分解する
;;   ・循環部分の範囲は記号で囲われていること
;;       例. "0.123{45}"
;;   num-st           数値文字列
;;   radix            入力の基数
;;   extended-range?  UnicodeのNdカテゴリの文字を受け付けるかどうか
(define (%split-recdec-str num-st radix extended-range?)
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
              (cond
               ((eqv? (digit->integer c radix extended-range?) 0)
                (set! zero-flag #t))
               (else (set! int-index i) (inc! mode) (loop))))
             ;; 整数部のチェック
             ((2)
              (cond
               ((digit->integer c radix extended-range?))
               ((eqv? c #\.) (set! frac-index i) (inc! mode))
               (else (set! err-flag #t))))
             ;; 小数部のチェック
             ((3)
              (cond
               ((digit->integer c radix extended-range?))
               ((eqv? c *rec-start-char*) (set! rec-index i) (inc! mode))
               (else (set! err-flag #t))))
             ;; 循環小数部のチェック
             ((4)
              (cond
               ((digit->integer c radix extended-range?))
               ((eqv? c *rec-end-char*) (set! end-index i) (set! mode 10))
               (else (set! err-flag #t))))
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


