(ns intrinsic-function.core-test
  (:require [clojure.test :refer :all]
            [intrinsic-function.core :refer :all]
            [clojure.zip :as z]
            [clojure.spec.alpha :as s])
  (:import [intrinsic_function.core   ;; note underscore!
            nap     pars    par
            hear    say     channel
            repeat-]))


;;  ___                           __       ___      _       _
;; | _ \___ _ _  __ _ _ __  ___  / _|___  / __|_  _| |__ __| |_
;; |   / -_) ' \/ _` | '  \/ -_) > _|_ _| \__ \ || | '_ (_-<  _|
;; |_|_\___|_||_\__,_|_|_|_\___| \_____|  |___/\_,_|_.__/__/\__|


(deftest renaming-test
  (testing "renaming")
  (is (= (rename-bound whisper-boat-2 'x 'g1234)
         (channel.
          'g1234
          (pars.
           [(say.  'g1234 'z (nap.))
            (hear. 'g1234 'y
                   (say. 'y 'g1234
                         (hear. 'g1234 'y (nap.))))
            (hear. 'z 'v
                   (say. 'v 'v (nap.)))])))))


;;   ___ _    _ _    _
;;  / __| |_ (_) |__| |_ _ ___ _ _
;; | (__| ' \| | / _` | '_/ -_) ' \
;;  \___|_||_|_|_\__,_|_| \___|_||_|


(deftest children-test
  (testing "every 'children' is a vector"
    (is (empty? (children (nap.))))
    (is (= 2 (count (children (par. (nap.) (nap.))))))
    (is (= 3 (count (children (pars. [(nap.) (nap.) (nap.)])))))
    (is (= 1 (count (children (hear. 'x 'y (nap.))))))
    (is (= 1 (count (children (say.  'z 'v (nap.))))))
    (is (= 1 (count (children (channel. 'x (nap.))))))
    (is (= 1 (count (children (repeat-. (nap.))))))
    (is (= 0 (count (children (pars. [])))))
    (is (= 1 (count (children (pars. [(nap.)])))))))


(children whisper-boat)
;; => [{:K {:chan x, :msg z, :K {}},
;;      :L
;;      {:K {:chan x, :msg y, :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}},
;;       :L {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}}}]
(children kit-2)
;; => [{:chan y, :msg x, :K {:chan x, :msg y, :K {}}}]
(map children (children whisper-boat-2))
;; => [{:kits
;;      [{:chan x, :msg z, :K {}}
;;       {:chan x, :msg y, :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;       {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}]}]


;;  ___
;; / __|_ __  ___ __ ___
;; \__ \ '_ \/ -_) _(_-<
;; |___/ .__/\___\__/__/
;;     |_|


(deftest pars-spec-test
  (testing "spec of pars"
    (let [ifc-pars :intrinsic-function.core/pars]
      (is (s/valid? ifc-pars (pars. [])))
      (is (s/valid? ifc-pars (pars. [(nap.)])))
      (is (not (s/valid? ifc-pars (pars. (nap.)))))
      (let [pnap (pars. [(nap.)])]
        (is (= pnap
               (s/conform ifc-pars pnap))))
      (is (s/invalid?
           (s/conform ifc-pars (pars. (nap.))))))))


(deftest flat-kit-test
  (testing "spec of flat-kit"
    (let [ifc-fk :intrinsic-function.core/flat-kit]
      (is (s/valid? ifc-fk whisper-boat-2))
      (is (not (s/valid? ifc-fk whisper-boat))))))


;;  _____           ___
;; |_   _|__ _ __  | _ \__ _ _ _ ___
;;   | |/ _ \ '_ \ |  _/ _` | '_(_-<
;;   |_|\___/ .__/ |_| \__,_|_| /__/
;;          |_|


(deftest top-pars-test
  (testing "top-pars"
    (is (empty? (find-top-pars kit-1)))
    (is (empty? (find-top-pars kit-2)))
    (is (empty? (find-top-pars kit-3)))
    (is (= (:K whisper-boat-2)
           (:top-pars (find-top-pars whisper-boat-2))))))


(deftest flat-kit-spec-test
  (is (thrown? java.lang.Exception
               (find-top-pars whisper-boat)))
  (is (find-top-pars whisper-boat-2)))


(deftest top-says-hears-test
  (testing "top-says-and-hears"
    (let [tsh-2 (find-top-says-and-hears whisper-boat-2)]
      (is (= 2 (count (:hears tsh-2))))
      (is (= 1 (count (:says  tsh-2)))))
    (is (->> [kit-1 kit-2 kit-3]
             (map find-top-says-and-hears)
             (every? empty?)))))


;;   ___             _               _
;;  / __|___ _ _  __| |_ _ _ _  _ __| |_ ___ _ _ ___
;; | (__/ _ \ ' \(_-<  _| '_| || / _|  _/ _ \ '_(_-<
;;  \___\___/_||_/__/\__|_|  \_,_\__|\__\___/_| /__/


(deftest construct-par
  (testing "constructing a par, two different ways (see https://stackoverflow.com/questions/9229434/maps-and-records-equality-in-clojure)."
    (is (= {:K 'P, :L 'Q}
           {:K 'P, :L 'Q}))
    (is (.equals {:K 'P, :L 'Q}
                 (->par 'P 'Q)))        ; one way
    (is (.equals {:K 'P, :L 'Q}
                 (par. 'P 'Q))))) ; another way


(deftest construct-others
  (testing  "constructing others"
    (is (nap.))
    (is (.equals {:chan 'x, :msg 'y, :K 'p}
                 (hear. 'x 'y 'p)))))


;;  _  _
;; | \| |__ _ _ __  ___ ___
;; | .` / _` | '  \/ -_|_-<
;; |_|\_\__,_|_|_|_\___/__/


(deftest free-names-test
  (testing "free names"
    (is (= #{}      (free-names (nap.))))
    (is (= #{'x 'y} (free-names (say. 'x 'y (nap.)))))
    (is (= #{'x 'z} (free-names kit-1)))
    (is (= #{'x}    (free-names kit-2)))
    (is (= #{'z}    (free-names kit-3)))
    (is (= #{'z}    (free-names whisper-boat)))
    (is (= #{'z}    (free-names whisper-boat-2)))
    (is (= #{}      (free-names (pars. []))))
    (is (= (free-names whisper-boat)
           (free-names (channel. 'x (pars. [kit-1 kit-2 kit-3])))))
    (is (= (free-names whisper-boat)
           (free-names (channel. 'x (pars. [kit-2 kit-3 kit-1])))))
    (is (= (free-names whisper-boat)
           (free-names (channel. 'x (pars. [kit-3 kit-1 kit-2])))))
    (is (= (free-names whisper-boat)
           (free-names (channel. 'x (pars. [kit-2 kit-1 kit-3])))))
    (is (= (free-names whisper-boat)
           (free-names (channel. 'x (pars. [kit-1 kit-3 kit-2])))))
    (is (= (free-names whisper-boat)
           (free-names (channel. 'x (pars. [kit-3 kit-2 kit-1])))))

    (is (= (free-names whisper-boat)
           (free-names (channel.
                        'x
                        (pars. [kit-1
                                (pars. [kit-2 kit-3])])))))

    (is (= (free-names whisper-boat)
           (free-names (channel.
                        'x
                        (pars. [kit-1
                                (par. kit-2 kit-3)])))))

    (is (= (free-names whisper-boat)
           (free-names (channel.
                        'x
                        (par. (pars. [kit-1 kit-2])
                              kit-3)))))

    (is (= (free-names whisper-boat)
           (free-names (channel.
                        'x
                        (pars. [(pars. [kit-1 kit-2])
                                kit-3])))))

    (is (= (free-names whisper-boat)
           (free-names (channel.
                        'x
                        (pars. [(par. kit-1 kit-2)
                                kit-3])))))))


(deftest bound-names-test
  (testing "bound names"
    (is (= #{}         (bound-names (nap.))))
    (is (= #{}         (bound-names (say. 'x 'y (nap.)))))
    (is (= #{}         (bound-names kit-1)))
    (is (= #{'y}       (bound-names kit-2)))
    (is (= #{'v}       (bound-names kit-3)))
    (is (= #{'x 'y 'v} (bound-names whisper-boat)))
    (is (= #{'x 'y 'v} (bound-names whisper-boat-2)))
    (is (= #{}         (bound-names (pars. []))))
    (is (= (bound-names whisper-boat)
           (bound-names (channel. 'x (pars. [kit-1 kit-2 kit-3])))))
    (is (= (bound-names whisper-boat)
           (bound-names (channel. 'x (pars. [kit-2 kit-3 kit-1])))))
    (is (= (bound-names whisper-boat)
           (bound-names (channel. 'x (pars. [kit-3 kit-1 kit-2])))))
    (is (= (bound-names whisper-boat)
           (bound-names (channel. 'x (pars. [kit-2 kit-1 kit-3])))))
    (is (= (bound-names whisper-boat)
           (bound-names (channel. 'x (pars. [kit-1 kit-3 kit-2])))))
    (is (= (bound-names whisper-boat)
           (bound-names (channel. 'x (pars. [kit-3 kit-2 kit-1])))))

    (is (= (bound-names whisper-boat)
           (bound-names (channel.
                         'x
                         (pars. [kit-1
                                 (pars. [kit-2 kit-3])])))))

    (is (= (bound-names whisper-boat)
           (bound-names (channel.
                         'x
                         (pars. [kit-1
                                 (par. kit-2 kit-3)])))))

    (is (= (bound-names whisper-boat)
           (bound-names (channel.
                         'x
                         (par. kit-1
                               (par. kit-2 kit-3))))))

    (is (= (bound-names whisper-boat)
           (bound-names (channel.
                         'x
                         (pars. [(pars. [kit-1 kit-2])
                                 kit-3])))))

    (is (= (bound-names whisper-boat)
           (bound-names (channel.
                         'x
                         (pars. [(par. kit-1 kit-2)
                                 kit-3])))))))


;;  ___ _      _   _              ___
;; | __| |__ _| |_| |_ ___ _ _   | _ \__ _ _ _ ___
;; | _|| / _` |  _|  _/ -_) ' \  |  _/ _` | '_(_-<
;; |_| |_\__,_|\__|\__\___|_||_| |_| \__,_|_| /__/


(deftest flatten-pars-test
  (testing "flatten-pars on our witnesses"
    (is (par?  (:K whisper-boat)))
    (is (pars? (:K (flatten-pars whisper-boat))))
    (is (pars? (:K whisper-boat-2)))
    (is (pars? (:K (flatten-pars whisper-boat-2))))
    (testing "idempotency"
      (is (= whisper-boat-2
             (flatten-pars whisper-boat-2)))
      (is (= (flatten-pars whisper-boat)
             (flatten-pars whisper-boat-2))))
    (is (= kit-1  (flatten-pars kit-1)))
    (is (= kit-2  (flatten-pars kit-2)))
    (is (= kit-3  (flatten-pars kit-3)))
    (is (= (nap.) (flatten-pars (nap.))))
    (is (= (let [b (par. (par. kit-1 kit-2) kit-3)]
             (flatten-pars b))
           (let [b (par. kit-1 (par. kit-2 kit-3))]
             (flatten-pars b))
           (let [a (pars. [kit-1 kit-2 kit-3])]
             (flatten-pars a))))
    (let [k (say. 'x 'y (nap.))]
      (is (= k (flatten-pars k)))
      (is (= k (flatten-pars (flatten-pars k))))
      (let [l (hear. 'x 'z (nap.))]
        (is (= (pars. [k l])
               (flatten-pars (par. k l))))))))
