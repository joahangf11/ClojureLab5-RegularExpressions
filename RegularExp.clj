;----------------------------------------------------------
; Problem Set #7: Regular Expressions
; Date: May 12, 2023.
; Authors:
;          A01748222 Joahan Javier Garcia Fernandez
;          A01747811 Benjamín Alejandro Cruz Cervantes
;          A01754574 Luis Fernando De León Silva
;----------------------------------------------------------


(ns RegularExp
  (:require [clojure.test :refer [deftest is run-tests]]))

;1
;;; Regular expression:
(def c-identifier #"[a-zA-Z_]\w*")

;2
;;; Regular expression:
(def scheme-boolean #"#(t(rue)?|f(alse)?)")

;3
;;; Regular expression:
(def scheme-integer #"\d+|#(b[01]+|o[0-7]+|d\d+|x[0-9a-fA-F]+)")

;4
;;; Regular expression:
(def java-integer #"(0|[1-9]\d*|0[xX][0-9a-fA-F]+|0[0-7]+)[lL]?")

;5
;;; Regular expression:
;Or
; Si tiene punto puede no tener exp o type
; Si no tiene punto:
;   o tiene exp
;   o tiene type
(def java-float #"(\d+\.\d*|\.\d+)([Ee][+-]?\d+)?[FfDd]?|(\d+\.?\d*|\.\d+)([Ee][+-]?\d+)[FfDd]?|(\d+\.?\d*|\.\d+)([Ee][+-]?\d+)?[FfDd]")
;lookahead assertion
;(def java-float #"(?=.*[\.EeFfDd])(\d+\.?\d*|\.\d+)([Ee][+-]?\d+)?[FfDd]?")

;6
;;; Regular expression:
(def c-comment #"/[*](?:.|\n)*?[*]/")


;Tests

(deftest test-c-identifier
  (is (re-matches c-identifier "_"))
  (is (re-matches c-identifier "a"))
  (is (re-matches c-identifier "A"))
  (is (re-matches c-identifier "_an_identfier_42"))
  (is (re-matches c-identifier "_1234567890"))
  (is (re-matches c-identifier "___________"))
  (is (re-matches c-identifier "ThisIsAnIdentfier"))
  (is (not (re-matches c-identifier "")))
  (is (not (re-matches c-identifier "5")))
  (is (not (re-matches c-identifier "1234567890")))
  (is (not (re-matches c-identifier "#!@$^")))
  (is (not (re-matches c-identifier "_a_b_c_$"))))

(deftest test-scheme-boolean
  (is (re-matches scheme-boolean "#t"))
  (is (re-matches scheme-boolean "#f"))
  (is (re-matches scheme-boolean "#true"))
  (is (re-matches scheme-boolean "#false"))
  (is (not (re-matches scheme-boolean "t")))
  (is (not (re-matches scheme-boolean "f")))
  (is (not (re-matches scheme-boolean "true")))
  (is (not (re-matches scheme-boolean "false")))
  (is (not (re-matches scheme-boolean "()")))
  (is (not (re-matches scheme-boolean "0")))
  (is (not (re-matches scheme-boolean "T")))
  (is (not (re-matches scheme-boolean "F")))
  (is (not (re-matches scheme-boolean "#v")))
  (is (not (re-matches scheme-boolean "#truth")))
  (is (not (re-matches scheme-boolean "#falsy"))))

(deftest test-scheme-integer
  (is (re-matches scheme-integer "0"))
  (is (re-matches scheme-integer "24601"))
  (is (re-matches scheme-integer "#d1234567890"))
  (is (re-matches scheme-integer "#b10"))
  (is (re-matches scheme-integer "#o12345670"))
  (is (re-matches scheme-integer "#x1234567890abcdefABCDEF"))
  (is (not (re-matches scheme-integer "")))
  (is (not (re-matches scheme-integer "#123")))
  (is (not (re-matches scheme-integer "#da1234567890")))
  (is (not (re-matches scheme-integer "#b102")))
  (is (not (re-matches scheme-integer "#o123456780")))
  (is (not (re-matches scheme-integer
                       "#x1234567890abcdefgABCDEF"))))

(deftest test-java-integer
  (is (re-matches java-integer "0"))
  (is (re-matches java-integer "1234567890"))
  (is (re-matches java-integer "012345670"))
  (is (re-matches java-integer "0xabcdef1234567890ABCDEF"))
  (is (re-matches java-integer "0l"))
  (is (re-matches java-integer "1234567890l"))
  (is (re-matches java-integer "012345670L"))
  (is (re-matches java-integer "0Xabcde1234567890fABCDEFL"))
  (is (not (re-matches java-integer "")))
  (is (not (re-matches java-integer "L")))
  (is (not (re-matches java-integer "1a234567890")))
  (is (not (re-matches java-integer "0123456780")))
  (is (not (re-matches java-integer "0x1234567890abcdefgABCD"))))

(deftest test-java-float
  (is (re-matches java-float "1."))
  (is (re-matches java-float ".2"))
  (is (re-matches java-float "3e4"))
  (is (re-matches java-float "5e-6"))
  (is (re-matches java-float "7e+8"))
  (is (re-matches java-float "9F"))
  (is (re-matches java-float "1f"))
  (is (re-matches java-float "2D"))
  (is (re-matches java-float "3d"))
  (is (re-matches java-float "1.2"))
  (is (re-matches java-float "1234.E567"))
  (is (re-matches java-float "1234.E-567"))
  (is (re-matches java-float "1234.E+567"))
  (is (re-matches java-float ".1234E567"))
  (is (re-matches java-float ".1234E-567"))
  (is (re-matches java-float ".1234E+567"))
  (is (re-matches java-float "123."))
  (is (re-matches java-float ".4567"))
  (is (re-matches java-float "123.4567"))
  (is (re-matches java-float "123.4567E890"))
  (is (re-matches java-float "123.4567E-890"))
  (is (re-matches java-float "123.4567E+890"))
  (is (re-matches java-float "123.F"))
  (is (re-matches java-float ".4567f"))
  (is (re-matches java-float "123.4567D"))
  (is (re-matches java-float "123.4567E890d"))
  (is (re-matches java-float "123.4567E-890F"))
  (is (re-matches java-float "123.4567E+890f"))
  (is (re-matches java-float "1234e567"))
  (is (re-matches java-float "1234e-567"))
  (is (re-matches java-float "1234e+567"))
  (is (re-matches java-float "12345678F"))
  (is (re-matches java-float "12345678f"))
  (is (re-matches java-float "12345678D"))
  (is (re-matches java-float "12345678d"))
  (is (not (re-matches java-float ".")))
  (is (not (re-matches java-float "0")))
  (is (not (re-matches java-float "1234")))
  (is (not (re-matches java-float "-1234")))
  (is (not (re-matches java-float "e1234")))
  (is (not (re-matches java-float "E-1234")))
  (is (not (re-matches java-float "F")))
  (is (not (re-matches java-float "f")))
  (is (not (re-matches java-float "d")))
  (is (not (re-matches java-float "D")))
  (is (not (re-matches java-float "E")))
  (is (not (re-matches java-float "e")))
  (is (not (re-matches java-float "123..456")))
  (is (not (re-matches java-float "123.456.789")))
  (is (not (re-matches java-float "123456E78.90")))
  (is (not (re-matches java-float "123.456E78.90")))
  (is (not (re-matches java-float "123.456E78DF")))
  (is (not (re-matches java-float "-123.4567E890")))
  (is (not (re-matches java-float "+123.4567E890")))
  (is (not (re-matches java-float "0x1234")))
  (is (not (re-matches java-float "01234")))
  (is (not (re-matches java-float "123E"))))

(deftest test-c-comment
  (is (re-matches c-comment "/**/"))
  (is (re-matches c-comment "/*-*/"))
  (is (re-matches c-comment "/*\n*/"))
  (is (re-matches c-comment
                  "/***********
                   /*         *
                   /*         *
                   /***********/"))
  (is (= 3 (count (re-seq c-comment "/*********
                                      Comment 1
                                      *********/

                                     /*********
                                      Comment 2
                                      *********/

                                     /*********
                                      Comment 3
                                      *********/"))))
  (is (not (re-matches c-comment "/")))
  (is (not (re-matches c-comment "/*")))
  (is (not (re-matches c-comment "/**")))
  (is (not (re-matches c-comment "/*/")))
  (is (not (re-matches c-comment "//")))
  (is (not (re-matches c-comment "/** /")))
  (is (not (re-matches c-comment "******/")))
  (is (not (re-matches c-comment "/ * * * */"))))

;Run tests
(run-tests)