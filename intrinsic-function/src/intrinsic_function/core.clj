(ns intrinsic-function.core
  (:gen-class)
  (:require [clojure.spec.alpha            :as    s       ]
            [clojure.pprint                :refer [pprint]]
            [clojure.set                   :as    set     ]
            [blaster.clj-fstring           :refer [f-str] ]
            #_[clojure.zip                   :as    z       ]
            #_[clojure.data.zip              :as    dz      ]
            #_[clojure.spec.gen.alpha        :as    gen     ]
            #_[clojure.spec.test.alpha       :as    stest   ]
            #_[clojure.test.check.generators :as    tgen    ]
            #_[clojure.set                   :as    set     ]
            #_[pathetic.core                 :as    path    ]
            #_[asr.lpython                   :as    lpython]))


(s/check-asserts true)


;;; The main source of complexity is variadic pars. Otherwise,
;;; this is a direct transcription of the kitten calculus from the
;;; companion paper. See 'core-test.clj' for lots of samples.


;;  _  _                                  _               _
;; | \| |__ _ _ __  ___ ___  _ __ _ _ ___| |_ ___  __ ___| |
;; | .` / _` | '  \/ -_|_-< | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;; |_|\_\__,_|_|_|_\___/__/ | .__/_| \___/\__\___/\__\___/_|
;;                          |_|


(defprotocol Names
  (free-names  [this])
  (bound-names [this])
  (recursor    [this bound-or-free]))


;;  ___                                        _               _
;; | _ \___ _ _  __ _ _ __  ___   _ __ _ _ ___| |_ ___  __ ___| |
;; |   / -_) ' \/ _` | '  \/ -_) | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;; |_|_\___|_||_\__,_|_|_|_\___| | .__/_| \___/\__\___/\__\___/_|
;;                               |_|


(defprotocol Rename
  (rename [this, old-, new-]))


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
  (flatten-pars [this]))


;;   ___ _    _ _    _                             _               _
;;  / __| |_ (_) |__| |_ _ ___ _ _    _ __ _ _ ___| |_ ___  __ ___| |
;; | (__| ' \| | / _` | '_/ -_) ' \  | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;;  \___|_||_|_|_\__,_|_| \___|_||_| | .__/_| \___/\__\___/\__\___/_|
;;                                   |_|


(defprotocol Children
  (children [this]))


;;  ___      _   _                   _               _
;; | _ \__ _| |_| |_    _ __ _ _ ___| |_ ___  __ ___| |
;; |  _/ _` |  _| ' \  | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;; |_| \__,_|\__|_||_| | .__/_| \___/\__\___/\__\___/_|
;;                     |_|


(defprotocol Path
  (path-key [this]))


;;  _  ___ _   _               ___      _         _
;; | |/ (_) |_| |_ ___ _ _    / __|__ _| |__ _  _| |_  _ ___
;; | ' <| |  _|  _/ -_) ' \  | (__/ _` | / _| || | | || (_-<
;; |_|\_\_|\__|\__\___|_||_|  \___\__,_|_\__|\_,_|_|\_,_/__/
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

  Path

  (path-key [_] nil)

  Children

  (children [_]    [])

  Names  (free-names  [_] #{})  (bound-names [_] #{})

  Flatten

  (flatten-pars [this] this)

  Rename

  (rename [this, old-, new-]
    (assert (= sym old-))
    (name-. new-))

  Subst

  (subst    [this _ _] this)
  (patch-up [this _]   this))


;; -+-+-+-
;;  n a p
;; -+-+-+-


(defrecord nap     []

  Path

  (path-key [_] nil)

  Children

  (children [_]    [])

  Names  (free-names  [_] #{})  (bound-names [_] #{})

  Flatten

  (flatten-pars [this] this)

  Subst

  (subst    [this _ _] this)
  (patch-up [this _]   this))


;; -+-+-+-+-
;;  p a r s
;; -+-+-+-+-
;; variadic par


(defrecord pars    [kits]

  Path

  (path-key [_] :kits)

  Children

  (children [_]    kits)

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

  (flatten-pars [this]
    (pars. (vec (flatten (map flatten-pars kits))))))


;;; See https://clojure.org/guides/spec.
(s/def ::pars
  (s/and (s/keys :req-un [::kits])
         #(vector? (:kits %))))


;; -+-+-+-
;;  p a r
;; -+-+-+-
;; dyadic (canonical) par


(defrecord par     [K L]

  Path

  (path-key [_] nil) ; can only path to pars, not par

  Children

  (children [_]    [K L])

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

  ;; To flatten a par:
  ;; 1. Flatten each of its children, K & L, removing every par.
  ;; 2. Each child is either a pars or not. Iff a pars, its :kits
  ;;    are non-nil.
  ;; 3. Iff both are pars, concat their kits into a new pars.
  ;; 4. Iff one is pars and the other not, concat the non-pars
  ;;    with the kits of the pars.
  ;; 5. Else, kits are a vector of the two parts

  (flatten-pars [this]
    (let [kf  (flatten-pars K)   ; everything under is converted
          kfk (:kits kf)
          lf  (flatten-pars L)
          lfk (:kits lf)
          new-kits
          (cond
            (and (nil? kfk) (nil? lfk)) [kf lf]
            (nil? kfk)                  (vec (concat [kf] lfk))
            (nil? lfk)                  (vec (concat kfk [lf]))
            :else                       (vec (concat kfk lfk)))]
      (pars. new-kits))))


;; -+-+-+-+-
;;  h e a r
;; -+-+-+-+-


(defrecord hear    [chan msg K]

  Path

  (path-key [_] :K)

  Children

  (children [_]    [K])

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

  (flatten-pars [_]
    (hear. chan msg (flatten-pars K))))


;; -+-+-+-
;;  s a y
;; -+-+-+-


(defrecord say     [chan msg K]

  Path

  (path-key [_] :K)

  Children

  (children [_]    [K])

  Names

  (free-names [_]
    (set/union
     (set [chan msg])
     (free-names K)))
  (bound-names [_]
    (bound-names K))

  Flatten

  (flatten-pars [_]
    (say. chan msg (flatten-pars K))))


;; -+-+-+-+-+-+-+-
;;  c h a n n e l
;; -+-+-+-+-+-+-+-


(defrecord channel [x K]                ; like nu in the pi calculus

  Path

  (path-key [_] :K)

  Children

  (children [_]    [K])

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

  (flatten-pars [_]
    (channel. x (flatten-pars K))))


;; -+-+-+-+-+-+-+-
;;  r e p e a t -
;; -+-+-+-+-+-+-+-


(defrecord repeat- [K]  ; without hyphen, collides with built-in "repeat"

  Path

  (path-key [_] :K)

  Children

  (children [_]    [K])

  Names

  (free-names  [_] (free-names K))
  (bound-names [_] (bound-names K))

  Flatten

  (flatten-pars [_]
    (repeat-. (flatten-pars K))))


;;  ___ _      _     _  ___ _     ___
;; | __| |__ _| |_  | |/ (_) |_  / __|_ __  ___ __
;; | _|| / _` |  _| | ' <| |  _| \__ \ '_ \/ -_) _|
;; |_| |_\__,_|\__| |_|\_\_|\__| |___/ .__/\___\__|
;;                                   |_|


(s/def ::flat-kit
  (s/and #(not (instance? par %))
         #(every? (fn [child]
                    (s/valid? ::flat-kit child))
                  (children %))))


;;  _____                 ___            _ _         _
;; |_   _|  _ _ __  ___  | _ \_ _ ___ __| (_)__ __ _| |_ ___ ___
;;   | || || | '_ \/ -_) |  _/ '_/ -_) _` | / _/ _` |  _/ -_|_-<
;;   |_| \_, | .__/\___| |_| |_| \___\__,_|_\__\__,_|\__\___/__/
;;       |__/|_|


(def name-?   (partial instance? name-))
(def nap?     (partial instance? nap))
(def par?     (partial instance? par))
(def pars?    (partial instance? pars))
(def hear?    (partial instance? hear))
(def say?     (partial instance? say))
(def channel? (partial instance? channel))
(def repeat-? (partial instance? repeat-))

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


(let [pr (pars. [kit-1
                 (pars. [kit-2 kit-3])])
      mpr (mapcat :kits (:kits pr))
      pl (pars. [(pars. [kit-1 kit-2])
                 kit-3])
      mpl (mapcat :kits (:kits pl))
      ]
  {:mpr mpr, :mpl mpl})
;; => {:mpr
;;     ({:chan x,
;;       :msg y,
;;       :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;      {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}),
;;     :mpl
;;     ({:chan x, :msg z, :K {}}
;;      {:chan x,
;;       :msg y,
;;       :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}})}
;; => {:mpr
;;     (nil
;;      [{:chan x,
;;        :msg y,
;;        :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;       {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}]),
;;     :mpl
;;     ([{:chan x, :msg z, :K {}}
;;       {:chan x,
;;        :msg y,
;;        :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}]
;;      nil)}


;;  __  __      _      _    _
;; |  \/  |__ _| |_ __| |_ (_)_ _  __ _
;; | |\/| / _` |  _/ _| ' \| | ' \/ _` |
;; |_|  |_\__,_|\__\__|_||_|_|_||_\__, |
;;                                |___/


(defn find-top-pars

  ([flat-kit, path-so-far]
   (if (instance? pars flat-kit)
     {:path path-so-far,
      :top-pars flat-kit}
     (let [cs (children flat-kit)
           ps (map #(find-top-pars % path-so-far) cs)]
       (if (empty? cs)
         ()
         (let [fp (first ps)]
           (if (and fp (not (empty? fp)))
             {:path (conj path-so-far (path-key flat-kit))
              :top-pars (:top-pars fp)}
             () ))))))

  ([flat-kit]
   {:pre [(s/assert ::flat-kit flat-kit)]}
   (find-top-pars flat-kit [])))


(find-top-pars whisper-boat-2)
;; => {:path [:K],
;;     :top-pars
;;     {:kits
;;      [{:chan x, :msg z, :K {}}
;;       {:chan x,
;;        :msg y,
;;        :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;       {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}]}}


(defn find-top-says-and-hears
  [flat-kit]
  (let [fop   (find-top-pars flat-kit)
        ps    (:kits (:top-pars fop))
        says  (filter (partial instance? say)  ps)
        hears (filter (partial instance? hear) ps)]
    (if (not (empty? fop))
      (assoc fop :says says, :hears hears)
      ())))


(find-top-says-and-hears whisper-boat-2)
;; => {:path [:K],
;;     :top-pars
;;     {:kits
;;      [{:chan x, :msg z, :K {}}
;;       {:chan x,
;;        :msg y,
;;        :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;       {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}]},
;;     :says ({:chan x, :msg z, :K {}}),
;;     :hears
;;     ({:chan x,
;;       :msg y,
;;       :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;      {:chan z, :msg v, :K {:chan v, :msg v, :K {}}})}


(defn non-deterministic-say-hear-match
  [flat-kit]
  (let [tsh (find-top-says-and-hears flat-kit)]
    (if (and (not (empty? tsh))
             (not (empty? (:says tsh)))
             (not (empty? (:hears tsh))))
      (let [match-say (first (:says tsh)) ; could be any
            match-hear (first (filter
                               #(= (:chan match-say)
                                   (:chan %))
                               (:hears tsh)))]
        (assoc tsh :match-say match-say, :match-hear match-hear)
        ))))

(non-deterministic-say-hear-match
 whisper-boat-2)
;; => {:path [:K],
;;     :top-pars
;;     {:kits
;;      [{:chan x, :msg z, :K {}}
;;       {:chan x,
;;        :msg y,
;;        :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;       {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}]},
;;     :says ({:chan x, :msg z, :K {}}),
;;     :hears
;;     ({:chan x,
;;       :msg y,
;;       :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
;;      {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}),
;;     :match-say {:chan x, :msg z, :K {}},
;;     :match-hear
;;     {:chan x,
;;      :msg y,
;;      :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}}


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
