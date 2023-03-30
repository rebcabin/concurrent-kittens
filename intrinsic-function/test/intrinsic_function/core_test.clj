(ns intrinsic-function.core-test
  (:require [clojure.test :refer :all]
            [intrinsic-function.core :refer :all])
  (:import [intrinsic_function.core
            nap
            hear
            say
            par
            channel
            repeat-])  ;; note underscore!
  )


(deftest construct-par
  (testing "constructing a par (see https://stackoverflow.com/questions/9229434/maps-and-records-equality-in-clojure)."
    (is (= {:K 'P, :L 'Q}
           {:K 'P, :L 'Q}))
    (is (.equals {:K 'P, :L 'Q}
                 (->par 'P 'Q)))
    (is (.equals {:K 'P, :L 'Q}
                 (par. 'P 'Q)))))


(deftest construct-others
  (testing  "constructing others"
    (is (nap.))
    (is (.equals {:chan 'x, :msg 'y, :K 'p}
                 (hear. 'x 'y 'p)))))


;; __   __               _      ___            _
;; \ \ / /__ _ _ _  _ __( )___ | _ ) ___  __ _| |_ ___
;;  \ V / -_) ' \ || (_-</(_-< | _ \/ _ \/ _` |  _(_-<
;;   \_/\___|_||_\_,_/__/ /__/ |___/\___/\__,_|\__/__/


(def -kit-1
  (say. 'x 'z (nap.)))


(def -kit-2
  (hear. 'x 'y
   (say. 'y 'x
    (hear. 'x 'y (nap.)))))


(def -kit-3
  (hear. 'z 'v
   (say. 'v 'v (nap.))))


(def -whisper-boat
  (channel. 'x
   (par. -kit-1
    (par. -kit-2 -kit-3))))


(def -parl-123 (par. (par. -kit-1 -kit-2) -kit-3))
(def -parr-123 (par. -kit-1 (par. -kit-2 -kit-3)))


(deftest parl-parr-test
  (testing "par-left and par-right"
    (is (parl? -parl-123))
    (is (not (parl? -parr-123)))
    (is (not (parr? -parl-123)))
    (is (parr? parr-123))))


(deftest free-names-test
  (testing "free names"
    (is (= #{} (free-names (nap.))))
    (is (= #{'x 'y} (free-names (say. 'x 'y (nap.)))))
    (is (= #{'x 'z} (free-names -kit-1)))
    (is (= #{'x}    (free-names -kit-2)))
    (is (= #{'z}    (free-names -kit-3)))
    (is (= #{'z}    (free-names -whisper-boat)))))


(bound-names -whisper-boat)


(deftest bound-names-test
  (testing "bound names"
    (is (= #{}         (bound-names -kit-1)))
    (is (= #{'y}       (bound-names -kit-2)))
    (is (= #{'v}       (bound-names -kit-3)))
    (is (= #{'x 'y 'v} (bound-names -whisper-boat)))))
