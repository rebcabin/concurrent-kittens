(ns intrinsic-function.core-test
  (:require [clojure.test :refer :all]
            [intrinsic-function.core :refer :all]
            [clojure.zip :as z]
            [clojure.spec.alpha :as s])
  (:import [intrinsic_function.core   ;; note underscore!
            name-
            nap
            pars
            par
            hear
            say
            channel
            repeat-]))


;;   ___ _    _ _    _
;;  / __| |_ (_) |__| |_ _ ___ _ _
;; | (__| ' \| | / _` | '_/ -_) ' \
;;  \___|_||_|_|_\__,_|_| \___|_||_|


(deftest children-test
  (testing "every 'children' is a vector"
    (is (empty? (children (name-. 'x))))
    (is (empty? (children (nap.))))
    (is (= 2 (count (children (par. (nap.) (nap.))))))
    (is (= 3 (count (children (pars. [(nap.) (nap.) (nap.)])))))
    (is (= 1 (count (children (hear. 'x 'y (nap.))))))
    (is (= 1 (count (children (say.  'z 'v (nap.))))))
    (is (= 1 (count (children (channel. 'x (nap.))))))
    (is (= 1 (count (children (repeat-. (nap.))))))
    (is (= 0 (count (children (pars. [])))))
    (is (= 1 (count (children (pars. [(nap.)])))))))


;;  ___               ___
;; | _ \__ _ _ _ ___ / __|_ __  ___ __
;; |  _/ _` | '_(_-< \__ \ '_ \/ -_) _|
;; |_| \__,_|_| /__/ |___/ .__/\___\__|
;;                       |_|


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


;;   ___       _                        _     ___
;;  / _ \ _  _| |_ ___ _ _ _ __  ___ __| |_  | _ \__ _ _ _ ___
;; | (_) | || |  _/ -_) '_| '  \/ _ (_-<  _| |  _/ _` | '_(_-<
;;  \___/ \_,_|\__\___|_| |_|_|_\___/__/\__| |_| \__,_|_| /__/


(deftest top-pars-test
  (testing "top-pars"
    (is (empty? (find-top-pars kit-1)))
    (is (empty? (find-top-pars kit-2)))
    (is (empty? (find-top-pars kit-3)))
    (is (= (:K whisper-boat-2)
           (:top-pars (find-top-pars whisper-boat-2))))))


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


(deftest flatten-vec-idempotency-test
  (testing "idempotency of flattening and vec'cing"
    (is (= (->> whisper-boat-2
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
                (map ->vec))))))


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


(deftest kits-vec-test
  (testing "one-level conversion of kits to vectors"

    (is (= (kit kit-1)
           'say))

    (is (= (kits-vec kit-1)
           '[[[:kit nap]]]))

    (is (= (kit kit-2)
           'hear))

    (is (= (kits-vec kit-2)
           '[[[:chan [[:name y] [:kit name]]]
              [:msg [[:name x] [:kit name]]]
              [:K
               [[:chan [[:name x] [:kit name]]]
                [:msg [[:name y] [:kit name]]]
                [:K [[:kit nap]]]
                [:kit hear]]]
              [:kit say]]]))

    (is (= (kits-vec kit-3)
           '[[[:chan [[:name v] [:kit name]]]
              [:msg [[:name v] [:kit name]]]
              [:K [[:kit nap]]]
              [:kit say]]]))

    (is (= (kits-vec whisper-boat)
           '[[[:K
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
              [:kit par]]]))

    (is (= (kits-vec whisper-boat-2)
           '[[[:kits
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
              [:kit pars]]]))

    (is (= (-> whisper-boat-2 kits-vec)
           (-> whisper-boat-2 flatten-pars kits-vec)))))


;;  _____
;; |_  (_)_ __ _ __  ___ _ _ ___
;;  / /| | '_ \ '_ \/ -_) '_(_-<
;; /___|_| .__/ .__/\___|_| /__/
;;       |_|  |_|


(deftest a-zipper-test
  (testing "equality of zippers with and without par-flattening"
    (is (= (->zip whisper-boat)
           (->zip whisper-boat-2)))))


(deftest lookup-test
  (testing "looking up attributes in a zipper"))


(->> whisper-boat-2
     ->zip
     z/root)
;; => [[:x [[:name x] [:kit name]]]
;;     [:K
;;      [[:kits
;;        ([[:chan [[:name x] [:kit name]]]
;;          [:msg [[:name z] [:kit name]]]
;;          [:K [[:kit nap]]]
;;          [:kit say]]
;;         [[:chan [[:name x] [:kit name]]]
;;          [:msg [[:name y] [:kit name]]]
;;          [:K
;;           [[:chan [[:name y] [:kit name]]]
;;            [:msg [[:name x] [:kit name]]]
;;            [:K
;;             [[:chan [[:name x] [:kit name]]]
;;              [:msg [[:name y] [:kit name]]]
;;              [:K [[:kit nap]]]
;;              [:kit hear]]]
;;            [:kit say]]]
;;          [:kit hear]]
;;         [[:chan [[:name z] [:kit name]]]
;;          [:msg [[:name v] [:kit name]]]
;;          [:K
;;           [[:chan [[:name v] [:kit name]]]
;;            [:msg [[:name v] [:kit name]]]
;;            [:K [[:kit nap]]]
;;            [:kit say]]]
;;          [:kit hear]])]
;;       [:kit pars]]]
;;     [:kit channel]]
;; => [[:x [[:name x] [:kit name]]]
;;     {:l [],
;;      :pnodes
;;      [[[:x [[:name x] [:kit name]]]
;;        [:K
;;         [[:kits
;;           ([[:chan [[:name x] [:kit name]]]
;;             [:msg [[:name z] [:kit name]]]
;;             [:K [[:kit nap]]]
;;             [:kit say]]
;;            [[:chan [[:name x] [:kit name]]]
;;             [:msg [[:name y] [:kit name]]]
;;             [:K
;;              [[:chan [[:name y] [:kit name]]]
;;               [:msg [[:name x] [:kit name]]]
;;               [:K
;;                [[:chan [[:name x] [:kit name]]]
;;                 [:msg [[:name y] [:kit name]]]
;;                 [:K [[:kit nap]]]
;;                 [:kit hear]]]
;;               [:kit say]]]
;;             [:kit hear]]
;;            [[:chan [[:name z] [:kit name]]]
;;             [:msg [[:name v] [:kit name]]]
;;             [:K
;;              [[:chan [[:name v] [:kit name]]]
;;               [:msg [[:name v] [:kit name]]]
;;               [:K [[:kit nap]]]
;;               [:kit say]]]
;;             [:kit hear]])]
;;          [:kit pars]]]
;;        [:kit channel]]],
;;      :ppath nil,
;;      :r
;;      ([:K
;;        [[:kits
;;          ([[:chan [[:name x] [:kit name]]]
;;            [:msg [[:name z] [:kit name]]]
;;            [:K [[:kit nap]]]
;;            [:kit say]]
;;           [[:chan [[:name x] [:kit name]]]
;;            [:msg [[:name y] [:kit name]]]
;;            [:K
;;             [[:chan [[:name y] [:kit name]]]
;;              [:msg [[:name x] [:kit name]]]
;;              [:K
;;               [[:chan [[:name x] [:kit name]]]
;;                [:msg [[:name y] [:kit name]]]
;;                [:K [[:kit nap]]]
;;                [:kit hear]]]
;;              [:kit say]]]
;;            [:kit hear]]
;;           [[:chan [[:name z] [:kit name]]]
;;            [:msg [[:name v] [:kit name]]]
;;            [:K
;;             [[:chan [[:name v] [:kit name]]]
;;              [:msg [[:name v] [:kit name]]]
;;              [:K [[:kit nap]]]
;;              [:kit say]]]
;;            [:kit hear]])]
;;         [:kit pars]]]
;;       [:kit channel])}]


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
               (flatten-pars (par. k l))))))
    ))
