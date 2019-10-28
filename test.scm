;;
;; Test recdec
;;

(add-load-path "." :relative)
(use gauche.test)

(test-start "recdec")
(use recdec)
(test-module 'recdec)

(define-syntax expr-test
  (syntax-rules ()
    ((_ txt ans expr)
     (test* (string-append txt (if (equal? txt "") "" " ")
                           ": " (format "~s" 'expr)) ans expr))
    ((_ txt ans expr chk)
     (test* (string-append txt (if (equal? txt "") "" " ")
                           ": " (format "~s" 'expr)) ans expr chk))))

(test-section "real->recdec")
(expr-test "" "0"           (real->recdec   0))
(expr-test "" "1"           (real->recdec   1))
(expr-test "" "-1"          (real->recdec  -1))
(expr-test "" "10"          (real->recdec  10))
(expr-test "" "-10"         (real->recdec -10))
(expr-test "" "0"           (real->recdec   0.0))
(expr-test "" "0.1"         (real->recdec   0.1))
(expr-test "" "-0.1"        (real->recdec  -0.1))
(expr-test "" "2.5"         (real->recdec   5/2))
(expr-test "" "-2.5"        (real->recdec  -5/2))
(expr-test "" "0.{3}"       (real->recdec   1/3))
(expr-test "" "-0.{3}"      (real->recdec  -1/3))
(expr-test "" "0.{142857}"  (real->recdec   1/7))
(expr-test "" "-0.{142857}" (real->recdec  -1/7))
(expr-test "" "0.123{45}"   (real->recdec   679/5500))
(expr-test "" "-0.123{45}"  (real->recdec  -679/5500))
(expr-test "" "1.23{45}"    (real->recdec   679/550))
(expr-test "" "-1.23{45}"   (real->recdec  -679/550))
(expr-test "" "12.3{45}"    (real->recdec   679/55))
(expr-test "" "-12.3{45}"   (real->recdec  -679/55))

(test-section "real->recdec long pattern")
(expr-test "" "0.{000000004111111115222222226333333337444444448555555559666666670777777781888888893}"
          (real->recdec 1/243243243))
(expr-test "" "0.{000000000000000003934444444444444444483788888888888888889282333333333333333337267777777777777777817122222222222222222615666666666666666670601111111111111111150455555555555555555949}"
          (real->recdec 1/254165489974583451))
(expr-test "" "0.00{000102030405060708091011121314151617181920212223242526272829303132333435363738394041424344454647484950515253545556575859606162636465666768697071727374757677787980818283848586878889909192939495969799}"
          (real->recdec 1/980100))

(test-section "real->recdec error")
(expr-test "" (test-error <error>) (real->recdec "0"))
(expr-test "" (test-error <error>) (real->recdec 1+2i))
(expr-test "" (test-error <error>) (real->recdec +inf.0))
(expr-test "" (test-error <error>) (real->recdec +nan.0))

(test-section "recdec->real")
(expr-test ""   0           (recdec->real "0"))
(expr-test ""   1           (recdec->real "1"))
(expr-test ""  -1           (recdec->real "-1"))
(expr-test ""  10           (recdec->real "10"))
(expr-test "" -10           (recdec->real "-10"))
(expr-test ""   0           (recdec->real "0.0"))
(expr-test ""   1/10        (recdec->real "0.1"))
(expr-test ""  -1/10        (recdec->real "-0.1"))
(expr-test ""   5/2         (recdec->real "2.5"))
(expr-test ""  -5/2         (recdec->real "-2.5"))
(expr-test ""   1/3         (recdec->real "0.{3}"))
(expr-test ""  -1/3         (recdec->real "-0.{3}"))
(expr-test ""   1/7         (recdec->real "0.{142857}"))
(expr-test ""  -1/7         (recdec->real "-0.{142857}"))
(expr-test ""   679/5500    (recdec->real "0.123{45}"))
(expr-test ""  -679/5500    (recdec->real "-0.123{45}"))
(expr-test ""   679/550     (recdec->real "1.23{45}"))
(expr-test ""  -679/550     (recdec->real "-1.23{45}"))
(expr-test ""   679/55      (recdec->real "12.3{45}"))
(expr-test ""  -679/55      (recdec->real "-12.3{45}"))

(test-section "real->recdec long pattern")
(expr-test "" 1/243243243
           (recdec->real "0.{000000004111111115222222226333333337444444448555555559666666670777777781888888893}"))
(expr-test "" 1/254165489974583451
           (recdec->real "0.{000000000000000003934444444444444444483788888888888888889282333333333333333337267777777777777777817122222222222222222615666666666666666670601111111111111111150455555555555555555949}"))
(expr-test "" 1/980100
           (recdec->real "0.00{000102030405060708091011121314151617181920212223242526272829303132333435363738394041424344454647484950515253545556575859606162636465666768697071727374757677787980818283848586878889909192939495969799}"))

(test-section "recdec->real error")
(expr-test "" (test-error <error>) (recdec->real 0))
(expr-test "" (test-error <error>) (recdec->real ""))
(expr-test "" (test-error <error>) (recdec->real "."))
(expr-test "" (test-error <error>) (recdec->real "+."))
(expr-test "" (test-error <error>) (recdec->real "+.{"))
(expr-test "" (test-error <error>) (recdec->real "+.{}"))

;; summary
(format (current-error-port) "~%~a" ((with-module gauche.test format-summary)))

(test-end)

