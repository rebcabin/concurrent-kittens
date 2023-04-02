(ns intrinsic-function.core
  (:gen-class)
  (:require [clojure.spec.alpha            :as    s       ]
            [clojure.pprint                :refer [pprint]]
            [clojure.set                   :as    set     ]
            [clojure.zip                   :as    z       ]
            #_[clojure.data.zip              :as    dz      ]
            #_[clojure.spec.gen.alpha        :as    gen     ]
            #_[clojure.spec.test.alpha       :as    stest   ]
            #_[clojure.test.check.generators :as    tgen    ]
            #_[clojure.set                   :as    set     ]
            #_[pathetic.core                 :as    path    ]
            #_[asr.lpython                   :as    lpython]))


;;; The main source of complexity is variadic pars. Otherwise,
;;; this is a direct transcription of the kitten calculus from the
;;; companion paper. See 'core-test.clj' for lots of samples.


;; __   __                     _               _
;; \ \ / /__ __   _ __ _ _ ___| |_ ___  __ ___| |
;;  \ V / -_) _| | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;;   \_/\___\__| | .__/_| \___/\__\___/\__\___/_|
;;               |_|
;;
;; to support zippers


(defprotocol Vec
  (->vec [this]))


;;  _  _                                  _               _
;; | \| |__ _ _ __  ___ ___  _ __ _ _ ___| |_ ___  __ ___| |
;; | .` / _` | '  \/ -_|_-< | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;; |_|\_\__,_|_|_|_\___/__/ | .__/_| \___/\__\___/\__\___/_|
;;                          |_|


(defprotocol Names
  (free-names  [this])
  (bound-names [this])
  (recursor    [this bound-or-free]))


;;  __  __      _      _    _
;; |  \/  |__ _| |_ __| |_ (_)_ _  __ _
;; | |\/| / _` |  _/ _| ' \| | ' \/ _` |
;; |_|  |_\__,_|\__\__|_||_|_|_||_\__, |
;;                                |___/


(defn match-up
  [say, hear]
  (= (:chan say) (:chan hear)))


;;  ___      _       _                  _               _
;; / __|_  _| |__ __| |_   _ __ _ _ ___| |_ ___  __ ___| |
;; \__ \ || | '_ (_-<  _| | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;; |___/\_,_|_.__/__/\__| | .__/_| \___/\__\___/\__\___/_|
;;                        |_|


(defprotocol Subst
  (patch-up [this x])
  (subst    [this x y]))


;;  ___ _      _   _                           _               _
;; | __| |__ _| |_| |_ ___ _ _    _ __ _ _ ___| |_ ___  __ ___| |
;; | _|| / _` |  _|  _/ -_) ' \  | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;; |_| |_\__,_|\__|\__\___|_||_| | .__/_| \___/\__\___/\__\___/_|
;;                               |_|


(defprotocol Flatten
  (par->vec [this])
  (repars   [this]))


(defn flatten-pars [kit]
  (-> kit
      par->vec
      repars))


;;  _   _ _   _                      _         _
;; | |_(_) |_| |_ ___ _ _    __ __ _| |__ _  _| |_  _ ___
;; | / / |  _|  _/ -_) ' \  / _/ _` | / _| || | | || (_-<
;; |_\_\_|\__|\__\___|_||_| \__\__,_|_\__|\_,_|_|\_,_/__/
;;
;; The "type" of a kitten is called a "kit." I didn't want the
;; heavily over-used words "type," "kind," "sort," etc. A "kit"
;; can be one of :nap, :pars, :par, :hear, :say, :channel,
;; :repeat-. :Pars is not in the kitten grammar in the little PDF.
;; Pars is just a flattened nest of one or more par's. But it
;; might as well be part of the grammar. It's not fundamentally a
;; different kind of thing. The children of a pars are its "kits."
;;
;; We'll add :name to the list of :kit's and to the protocols. By
;; doing so, we expose the inconvenient fact that names are not
;; part of the kitten grammar, even though we call them a "kit,"
;; as a kludge, so we can serialize them to vectors for the zipper
;; library. They're an undefined, outer kind of thing that come
;; from an infinite well. This inconvenient fact is exactly
;; the "kludge" that RhoLang solves.


;; -+-+-+-+-+-
;;  n a m e -
;; -+-+-+-+-+-


(defrecord name-   [sym]

  Vec

  (->vec [_]    [[:name sym] [:kit 'name]])

  Names  (free-names  [_] #{})  (bound-names [_] #{})

  Flatten

  (par->vec [this]    this)
  (repars   [this]    this)

  Subst

  (subst    [this _ _] this)
  (patch-up [this _]   this))


;; -+-+-+-
;;  n a p
;; -+-+-+-


(defrecord nap     []

  Vec

  (->vec [this]
    (conj (vec this) [:kit 'nap]))

  Names  (free-names  [_] #{})  (bound-names [_] #{})

  Flatten

  (par->vec [this]    this)
  (repars   [this]    this)

  Subst

  (subst    [this _ _] this)
  (patch-up [this _]   this))


;; -+-+-+-+-
;;  p a r s
;; -+-+-+-+-
;; variadic par


(defrecord pars    [kits]

  Vec

  (->vec [_]
    (conj [[:kits (map ->vec kits)]]
          [:kit  'pars]))

  Names

  (recursor [_ bf]
    (loop [result #{}, forms kits]
      (if (not (empty? forms))          ; "if forms" does not work
        (recur (into result (bf (first forms)))
               (rest forms))
        result)))

  (free-names  [this]  (recursor this free-names))
  (bound-names [this]  (recursor this bound-names))

  Flatten

  (par->vec [this]  (flatten (vec (map par->vec kits))))
  (repars   [_]     (throw (java.lang.UnsupportedOperationException.
                            "can't repars a pars"))))


;; -+-+-+-
;;  p a r
;; -+-+-+-
;; dyadic (canonical) par


(defrecord par     [K L]

  Vec

  (->vec [this]
    (conj [[:K (->vec K)] [:L (->vec L)]]
          [:kit 'par]))

  Names

  (free-names [_]
    (set/union
     (free-names K)
     (free-names L)))
  (bound-names [_]
    (set/union
     (bound-names K)
     (bound-names L)))

  Flatten

  (par->vec [_]    (flatten [(par->vec K) (par->vec L)]))
  (repars   [_]    (throw (java.lang.UnsupportedOperationException.
                           "can't repars a par"))))


;; -+-+-+-+-
;;  h e a r
;; -+-+-+-+-


(defrecord hear    [chan msg K]

  Vec

  (->vec [this]
    (conj [[:chan (->vec (name-. chan))]
           [:msg  (->vec (name-. msg))]
           [:K    (->vec K)]]
          [:kit 'hear]))

  Names

  (free-names [_]
    (set/union
     #{chan}
     (set/difference
      (free-names K)
      #{msg})))
  (bound-names [_]
    (set/union
     #{msg}
     (bound-names K)))

  Subst  (patch-up [this x])

  Flatten

  (par->vec [_]    (hear. chan msg (par->vec K)))
  (repars   [this] (if (seq? K)
                     (hear. chan msg (pars. K))
                     this)))


;; -+-+-+-
;;  s a y
;; -+-+-+-


(defrecord say     [chan msg K]

  Vec

  (->vec [this]
    (conj [[:chan (->vec (name-. chan))]
           [:msg  (->vec (name-. msg))]
           [:K    (->vec K)]]
          [:kit 'say]))

  Names

  (free-names [_]
    (set/union
     (set [chan msg])
     (free-names K)))
  (bound-names [_]
    (bound-names K))

  Flatten

  (par->vec [_]    (say. chan msg (par->vec K)))
  (repars   [this] (if (seq? K)
                     (say. chan msg (pars. K))
                     this)))


;; -+-+-+-+-+-+-+-
;;  c h a n n e l
;; -+-+-+-+-+-+-+-


(defrecord channel [x K]                ; like nu in the pi calculus

  Vec

  (->vec [this]
    (conj [[:x (->vec (name-. x))]
           [:K (->vec K)]]
          [:kit 'say]))

  Names

  (free-names [_]
    (set/difference
     (free-names K)
     #{x}))
  (bound-names [_]
    (set/union
     #{x}
     (bound-names K)))

  Flatten

  (par->vec [_]     (channel. x (par->vec K)))
  (repars   [this]  (if (seq? K)
                      (channel. x (pars. K))
                      this)))


;; -+-+-+-+-+-+-+-
;;  r e p e a t -
;; -+-+-+-+-+-+-+-


(defrecord repeat- [K]         ; without hyphen, collides with built-in "repeat"

  Vec
  (->vec [this]
    (conj [[:K (->vec K)]]
          [:kit 'repeat-]))

  Names

  (free-names  [_] (free-names K))
  (bound-names [_] (bound-names K))

  Flatten

  (par->vec [_]     (repeat-. (par->vec K)))
  (repars   [this]  (if (seq? K)
                      (repeat-. (pars. K))
                      this)))


;; __   __               _      ___            _
;; \ \ / /__ _ _ _  _ __( )___ | _ ) ___  __ _| |_ ___
;;  \ V / -_) ' \ || (_-</(_-< | _ \/ _ \/ _` |  _(_-<
;;   \_/\___|_||_\_,_/__/ /__/ |___/\___/\__,_|\__/__/


;;; Support for the accompanying paper written in org-babel.
;;; Apparently need the following witnesses for org-babel
;;; (org-latex-export-to-pdf doesn't work well without them).


(def kit-1
  (say. 'x 'z (nap.)))
kit-1
;; => {:chan x, :msg z, :K {}}


(def kit-2
  (hear. 'x 'y
         (say. 'y 'x
               (hear. 'x 'y (nap.)))))
kit-2
;; => {:chan x,
;;     :msg y,
;;     :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}


(def kit-3
  (hear. 'z 'v
         (say. 'v 'v (nap.))))
kit-3
;; => {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}


(def whisper-boat
  (channel. 'x
            (par. kit-1
                  (par. kit-2 kit-3))))
whisper-boat
;; => {:x x,
;;     :K
;;     {:K {:chan x, :msg z, :K {}},
;;      :L
;;      {:K
;;       {:chan x,
;;        :msg y,
;;        :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}},
;;       :L {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}}}}


(def whisper-boat-2
  (channel. 'x
            (pars. [kit-1 kit-2 kit-3])))
whisper-boat-2
;; => {:x x,
;;     :K
;;     {:kits
;;      [{:chan x, :msg z, :K {}}
;;       {:chan x,
;;        :msg y,
;;        :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;       {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}]}}


;;  _____
;; |_  (_)_ __ _ __  ___ _ _ ___
;;  / /| | '_ \ '_ \/ -_) '_(_-<
;; /___|_| .__/ .__/\___|_| /__/
;;       |_|  |_|
;;
;; https://clojuredocs.org/clojure.zip/zipper


(->vec kit-1)
;; => [[:chan [[:name x] [:kit name]]]
;;     [:msg [[:name z] [:kit name]]]
;;     [:K [[:kit nap]]]
;;     [:kit say]]


(->vec kit-2)
;; => [[:chan [[:name x] [:kit name]]]
;;     [:msg [[:name y] [:kit name]]]
;;     [:K
;;      [[:chan [[:name y] [:kit name]]]
;;       [:msg [[:name x] [:kit name]]]
;;       [:K
;;        [[:chan [[:name x] [:kit name]]]
;;         [:msg [[:name y] [:kit name]]]
;;         [:K [[:kit nap]]]
;;         [:kit hear]]]
;;       [:kit say]]]
;;     [:kit hear]]


(->vec kit-3)
;; => [[:chan [[:name z] [:kit name]]]
;;     [:msg [[:name v] [:kit name]]]
;;     [:K
;;      [[:chan [[:name v] [:kit name]]]
;;       [:msg [[:name v] [:kit name]]]
;;       [:K [[:kit nap]]]
;;       [:kit say]]]
;;     [:kit hear]]


(->vec whisper-boat)
;; => [[:x [[:name x] [:kit name]]]
;;     [:K
;;      [[:K
;;        [[:chan [[:name x] [:kit name]]]
;;         [:msg [[:name z] [:kit name]]]
;;         [:K [[:kit nap]]]
;;         [:kit say]]]
;;       [:L
;;        [[:K
;;          [[:chan [[:name x] [:kit name]]]
;;           [:msg [[:name y] [:kit name]]]
;;           [:K
;;            [[:chan [[:name y] [:kit name]]]
;;             [:msg [[:name x] [:kit name]]]
;;             [:K
;;              [[:chan [[:name x] [:kit name]]]
;;               [:msg [[:name y] [:kit name]]]
;;               [:K [[:kit nap]]]
;;               [:kit hear]]]
;;             [:kit say]]]
;;           [:kit hear]]]
;;         [:L
;;          [[:chan [[:name z] [:kit name]]]
;;           [:msg [[:name v] [:kit name]]]
;;           [:K
;;            [[:chan [[:name v] [:kit name]]]
;;             [:msg [[:name v] [:kit name]]]
;;             [:K [[:kit nap]]]
;;             [:kit say]]]
;;           [:kit hear]]]
;;         [:kit par]]]
;;       [:kit par]]]
;;     [:kit say]]


(->vec whisper-boat-2)
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
;;     [:kit say]]


(->> whisper-boat-2
     :K
     :kits
     (map ->vec))
;; => ([[:chan [[:name x] [:kit name]]]
;;      [:msg [[:name z] [:kit name]]]
;;      [:K [[:kit nap]]]
;;      [:kit say]]
;;     [[:chan [[:name x] [:kit name]]]
;;      [:msg [[:name y] [:kit name]]]
;;      [:K
;;       [[:chan [[:name y] [:kit name]]]
;;        [:msg [[:name x] [:kit name]]]
;;        [:K
;;         [[:chan [[:name x] [:kit name]]]
;;          [:msg [[:name y] [:kit name]]]
;;          [:K [[:kit nap]]]
;;          [:kit hear]]]
;;        [:kit say]]]
;;      [:kit hear]]
;;     [[:chan [[:name z] [:kit name]]]
;;      [:msg [[:name v] [:kit name]]]
;;      [:K
;;       [[:chan [[:name v] [:kit name]]]
;;        [:msg [[:name v] [:kit name]]]
;;        [:K [[:kit nap]]]
;;        [:kit say]]]
;;      [:kit hear]])


;;   ___                     _   _
;;  / _ \ _ __  ___ _ _ __ _| |_(_)___ _ _  ___
;; | (_) | '_ \/ -_) '_/ _` |  _| / _ \ ' \(_-<
;;  \___/| .__/\___|_| \__,_|\__|_\___/_||_/__/
;;       |_|


;; -+-+-+-+-+-+-+-+-+-+-+-+-+-
;;  c o n v o l v e - p a r s
;; -+-+-+-+-+-+-+-+-+-+-+-+-+-


;; Confer Clojure's magnificent destructuring:
;; https://gist.github.com/john2x/e1dca953548bfdfb9844


(defn parl?
  "left-hugging par's"
  [{:keys [K L]}]
  (instance? par K))


(defn parr?
  "right-hugging par's"
  [{:keys [K L]}]
  (instance? par L))


(defn convolve-pars
  [{:keys [K L] :as input}]
  (cond (parl? input)
        (let [{Kl :K, Ll :L} K]
          (par. Kl (par. Ll L)))
        (parr? input)
        (let [{Kr :K, Lr :L} L]
          (par. (par. K Kr) Lr))
        :else input))


;;  _____
;; |_  (_)_ __ _ __  ___ _ _ ___
;;  / /| | '_ \ '_ \/ -_) '_(_-<
;; /___|_| .__/ .__/\___|_| /__/
;;       |_|  |_|


(defn ->zip [kit]
  (->> kit
       flatten-pars
       ->vec
       z/vector-zip))


(->zip kit-1)
;; => [[[:chan [[:name x] [:kit name]]]
;;      [:msg [[:name z] [:kit name]]]
;;      [:K [[:kit nap]]]
;;      [:kit say]]
;;     nil]


(->zip kit-2)
;; => [[[:chan [[:name x] [:kit name]]]
;;      [:msg [[:name y] [:kit name]]]
;;      [:K
;;       [[:chan [[:name y] [:kit name]]]
;;        [:msg [[:name x] [:kit name]]]
;;        [:K
;;         [[:chan [[:name x] [:kit name]]]
;;          [:msg [[:name y] [:kit name]]]
;;          [:K [[:kit nap]]]
;;          [:kit hear]]]
;;        [:kit say]]]
;;      [:kit hear]]
;;     nil]


(->zip kit-3)
;; => [[[:chan [[:name z] [:kit name]]]
;;      [:msg [[:name v] [:kit name]]]
;;      [:K
;;       [[:chan [[:name v] [:kit name]]]
;;        [:msg [[:name v] [:kit name]]]
;;        [:K [[:kit nap]]]
;;        [:kit say]]]
;;      [:kit hear]]
;;     nil]


(->zip whisper-boat-2)
;; => [[[:x [[:name x] [:kit name]]]
;;      [:K
;;       [[:kits
;;         ([[:chan [[:name x] [:kit name]]]
;;           [:msg [[:name z] [:kit name]]]
;;           [:K [[:kit nap]]]
;;           [:kit say]]
;;          [[:chan [[:name x] [:kit name]]]
;;           [:msg [[:name y] [:kit name]]]
;;           [:K
;;            [[:chan [[:name y] [:kit name]]]
;;             [:msg [[:name x] [:kit name]]]
;;             [:K
;;              [[:chan [[:name x] [:kit name]]]
;;               [:msg [[:name y] [:kit name]]]
;;               [:K [[:kit nap]]]
;;               [:kit hear]]]
;;             [:kit say]]]
;;           [:kit hear]]
;;          [[:chan [[:name z] [:kit name]]]
;;           [:msg [[:name v] [:kit name]]]
;;           [:K
;;            [[:chan [[:name v] [:kit name]]]
;;             [:msg [[:name v] [:kit name]]]
;;             [:K [[:kit nap]]]
;;             [:kit say]]]
;;           [:kit hear]])]
;;        [:kit pars]]]
;;      [:kit say]]
;;     nil]


;;  ___        _         _   _
;; | _ \___ __| |_  _ __| |_(_)___ _ _
;; |   / -_) _` | || / _|  _| / _ \ ' \
;; |_|_\___\__,_|\_,_\__|\__|_\___/_||_|


;; -+-+-+-+-+-+-+-+-
;;  M a t c h i n g
;; -+-+-+-+-+-+-+-+-


(defn find-outermost-par [kit]
  (let [zk (->zip kit)]
    )
  )


(find-outermost-par whisper-boat)


;; -+-+-+-+-+-+-+-+-
;;  R e n a m i n g
;; -+-+-+-+-+-+-+-+-


;; -+-+-+-+-+-+-+-+-+-+-+-+-
;;  S u b s t i t u t i o n
;; -+-+-+-+-+-+-+-+-+-+-+-+-


;; -+-+-+-+-+-+-+-+-
;;  G o b b l i n g
;; -+-+-+-+-+-+-+-+-


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (pprint (ns-publics 'intrinsic-function.core))
  (println "Hello, World!"))
