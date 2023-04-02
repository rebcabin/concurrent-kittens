(ns intrinsic-function.core-test
  (:require [clojure.test :refer :all]
            [intrinsic-function.core :refer :all])
  (:import [intrinsic_function.core   ;; note underscore!
            nap
            pars
            par
            hear
            say
            channel
            repeat-]))


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


;;  ___                         _   ___
;; | _ \__ _ _ _   __ _ _ _  __| | | _ \__ _ _ _ ___
;; |  _/ _` | '_| / _` | ' \/ _` | |  _/ _` | '_(_-<
;; |_| \__,_|_|   \__,_|_||_\__,_| |_| \__,_|_| /__/


(def parl-123 (par. (par. kit-1 kit-2) kit-3))
(def parr-123 (par. kit-1 (par. kit-2 kit-3)))


;;  _  _
;; | \| |__ _ _ __  ___ ___
;; | .` / _` | '  \/ -_|_-<
;; |_|\_\__,_|_|_|_\___/__/


(deftest parlparr-test
  (testing "par-left and par-right"
    (is (parl? parl-123))
    (is (not (parl? parr-123)))
    (is (not (parr? parl-123)))
    (is (parr? parr-123)))
  (testing "convolving"
    (is (= parr-123 (convolve-pars parl-123)))
    (is (= parl-123 (convolve-pars parr-123)))))


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


;;  _____
;; |_  (_)_ __ _ __  ___ _ _ ___
;;  / /| | '_ \ '_ \/ -_) '_(_-<
;; /___|_| .__/ .__/\___|_| /__/
;;       |_|  |_|
;;
;; https://clojuredocs.org/clojure.zip/zipper


(deftest ->vec-test
  (testing "conversion to vectors"

    (is (= (->vec kit-1)
           '[[:chan [[:name x] [:kit name]]]
             [:msg [[:name z] [:kit name]]]
             [:K [[:kit nap]]]
             [:kit say]]))

    (is (= (->vec kit-2)
           '[[:chan [[:name x] [:kit name]]]
             [:msg [[:name y] [:kit name]]]
             [:K
              [[:chan [[:name y] [:kit name]]]
               [:msg [[:name x] [:kit name]]]
               [:K
                [[:chan [[:name x] [:kit name]]]
                 [:msg [[:name y] [:kit name]]]
                 [:K [[:kit nap]]]
                 [:kit hear]]]
               [:kit say]]]
             [:kit hear]]))

    (is (= (->vec kit-3)
           '[[:chan [[:name z] [:kit name]]]
             [:msg [[:name v] [:kit name]]]
             [:K
              [[:chan [[:name v] [:kit name]]]
               [:msg [[:name v] [:kit name]]]
               [:K [[:kit nap]]]
               [:kit say]]]
             [:kit hear]]))

    (is (= (->vec whisper-boat)
           '[[:x [[:name x] [:kit name]]]
             [:K
              [[:K
                [[:chan [[:name x] [:kit name]]]
                 [:msg [[:name z] [:kit name]]]
                 [:K [[:kit nap]]]
                 [:kit say]]]
               [:L
                [[:K
                  [[:chan [[:name x] [:kit name]]]
                   [:msg [[:name y] [:kit name]]]
                   [:K
                    [[:chan [[:name y] [:kit name]]]
                     [:msg [[:name x] [:kit name]]]
                     [:K
                      [[:chan [[:name x] [:kit name]]]
                       [:msg [[:name y] [:kit name]]]
                       [:K [[:kit nap]]]
                       [:kit hear]]]
                     [:kit say]]]
                   [:kit hear]]]
                 [:L
                  [[:chan [[:name z] [:kit name]]]
                   [:msg [[:name v] [:kit name]]]
                   [:K
                    [[:chan [[:name v] [:kit name]]]
                     [:msg [[:name v] [:kit name]]]
                     [:K [[:kit nap]]]
                     [:kit say]]]
                   [:kit hear]]]
                 [:kit par]]]
               [:kit par]]]
             [:kit channel]]))

    (is (= (->vec whisper-boat-2)
           '[[:x [[:name x] [:kit name]]]
             [:K
              [[:kits
                ([[:chan [[:name x] [:kit name]]]
                  [:msg [[:name z] [:kit name]]]
                  [:K [[:kit nap]]]
                  [:kit say]]
                 [[:chan [[:name x] [:kit name]]]
                  [:msg [[:name y] [:kit name]]]
                  [:K
                   [[:chan [[:name y] [:kit name]]]
                    [:msg [[:name x] [:kit name]]]
                    [:K
                     [[:chan [[:name x] [:kit name]]]
                      [:msg [[:name y] [:kit name]]]
                      [:K [[:kit nap]]]
                      [:kit hear]]]
                    [:kit say]]]
                  [:kit hear]]
                 [[:chan [[:name z] [:kit name]]]
                  [:msg [[:name v] [:kit name]]]
                  [:K
                   [[:chan [[:name v] [:kit name]]]
                    [:msg [[:name v] [:kit name]]]
                    [:K [[:kit nap]]]
                    [:kit say]]]
                  [:kit hear]])]
               [:kit pars]]]
             [:kit channel]]))))


(deftest ->vec-extract-topmost-pars
  (testing "extraction of top-most pars from a term"
    (is (= (->> whisper-boat-2
                :K
                :kits
                (map ->vec))
           '([[:chan [[:name x] [:kit name]]]
              [:msg [[:name z] [:kit name]]]
              [:K [[:kit nap]]]
              [:kit say]]
             [[:chan [[:name x] [:kit name]]]
              [:msg [[:name y] [:kit name]]]
              [:K
               [[:chan [[:name y] [:kit name]]]
                [:msg [[:name x] [:kit name]]]
                [:K
                 [[:chan [[:name x] [:kit name]]]
                  [:msg [[:name y] [:kit name]]]
                  [:K [[:kit nap]]]
                  [:kit hear]]]
                [:kit say]]]
              [:kit hear]]
             [[:chan [[:name z] [:kit name]]]
              [:msg [[:name v] [:kit name]]]
              [:K
               [[:chan [[:name v] [:kit name]]]
                [:msg [[:name v] [:kit name]]]
                [:K [[:kit nap]]]
                [:kit say]]]
              [:kit hear]])))))


(deftest another-flattening-test
  (testing "a few more cases of flattening or pre-flattening"
    (= (->> whisper-boat-2
            :K
            :kits
            (map ->vec))
       (->> whisper-boat-2
            flatten-pars
            :K
            :kits
            (map ->vec))
       (->> whisper-boat
            flatten-pars
            :K
            :kits
            (map ->vec)))))


;; __   __
;; \ \ / /__ __   _ _ ___ _ __ ___
;;  \ V / -_) _| | '_/ -_) '_ (_-<
;;   \_/\___\__| |_| \___| .__/__/
;;                       |_|


(deftest vec-reps-test
  (testing "vec reps"
    (is (= (->vec kit-1))
        '[[:chan [[:name x] [:kit name]]]
          [:msg [[:name z] [:kit name]]]
          [:K [[:kit nap]]]
          [:kit say]])
    (is (= (->vec kit-2)
           '[[:chan [[:name x] [:kit name]]]
             [:msg [[:name y] [:kit name]]]
             [:K
              [[:chan [[:name y] [:kit name]]]
               [:msg [[:name x] [:kit name]]]
               [:K
                [[:chan [[:name x] [:kit name]]]
                 [:msg [[:name y] [:kit name]]]
                 [:K [[:kit nap]]]
                 [:kit hear]]]
               [:kit say]]]
             [:kit hear]]))
    (is (= (->vec kit-3)
           '[[:chan [[:name z] [:kit name]]]
             [:msg [[:name v] [:kit name]]]
             [:K
              [[:chan [[:name v] [:kit name]]]
               [:msg [[:name v] [:kit name]]]
               [:K [[:kit nap]]]
               [:kit say]]]
             [:kit hear]]))
    (is (= (->vec whisper-boat)
           '[[:x [[:name x] [:kit name]]]
             [:K
              [[:K
                [[:chan [[:name x] [:kit name]]]
                 [:msg [[:name z] [:kit name]]]
                 [:K [[:kit nap]]]
                 [:kit say]]]
               [:L
                [[:K
                  [[:chan [[:name x] [:kit name]]]
                   [:msg [[:name y] [:kit name]]]
                   [:K
                    [[:chan [[:name y] [:kit name]]]
                     [:msg [[:name x] [:kit name]]]
                     [:K
                      [[:chan [[:name x] [:kit name]]]
                       [:msg [[:name y] [:kit name]]]
                       [:K [[:kit nap]]]
                       [:kit hear]]]
                     [:kit say]]]
                   [:kit hear]]]
                 [:L
                  [[:chan [[:name z] [:kit name]]]
                   [:msg [[:name v] [:kit name]]]
                   [:K
                    [[:chan [[:name v] [:kit name]]]
                     [:msg [[:name v] [:kit name]]]
                     [:K [[:kit nap]]]
                     [:kit say]]]
                   [:kit hear]]]
                 [:kit par]]]
               [:kit par]]]
             [:kit channel]]))
    (is (= (->vec whisper-boat-2)
           '[[:x [[:name x] [:kit name]]]
             [:K
              [[:kits
                ([[:chan [[:name x] [:kit name]]]
                  [:msg [[:name z] [:kit name]]]
                  [:K [[:kit nap]]]
                  [:kit say]]
                 [[:chan [[:name x] [:kit name]]]
                  [:msg [[:name y] [:kit name]]]
                  [:K
                   [[:chan [[:name y] [:kit name]]]
                    [:msg [[:name x] [:kit name]]]
                    [:K
                     [[:chan [[:name x] [:kit name]]]
                      [:msg [[:name y] [:kit name]]]
                      [:K [[:kit nap]]]
                      [:kit hear]]]
                    [:kit say]]]
                  [:kit hear]]
                 [[:chan [[:name z] [:kit name]]]
                  [:msg [[:name v] [:kit name]]]
                  [:K
                   [[:chan [[:name v] [:kit name]]]
                    [:msg [[:name v] [:kit name]]]
                    [:K [[:kit nap]]]
                    [:kit say]]]
                  [:kit hear]])]
               [:kit pars]]]
             [:kit channel]]))
    (is (= (->> whisper-boat-2
                :K
                :kits
                (map ->vec))
           (->> whisper-boat-2
                ->vec
                (into {})
                :K
                (into {})
                :kits)))))


;;  _____
;; |_  (_)_ __ _ __  ___ _ _ ___
;;  / /| | '_ \ '_ \/ -_) '_(_-<
;; /___|_| .__/ .__/\___|_| /__/
;;       |_|  |_|


(deftest a-zipper-test
  (testing "equality of zippers with and without par-flattening"
    (is (= (->zip whisper-boat)
           (->zip whisper-boat-2)))))


;;   ___                     _   _
;;  / _ \ _ __  ___ _ _ __ _| |_(_)___ _ _  ___
;; | (_) | '_ \/ -_) '_/ _` |  _| / _ \ ' \(_-<
;;  \___/| .__/\___|_| \__,_|\__|_\___/_||_/__/
;;       |_|


(deftest flatten-pars-test
  (testing "flatten-pars on our witnesses"
    (is (instance? par  (:K whisper-boat)))
    (is (instance? pars (:K (flatten-pars whisper-boat))))
    (is (instance? pars (:K whisper-boat-2)))
    (is (instance? pars (:K (flatten-pars whisper-boat-2))))
    (is (= whisper-boat-2 (flatten-pars whisper-boat-2)))
    (is (= (flatten-pars whisper-boat) (flatten-pars whisper-boat-2)))))
