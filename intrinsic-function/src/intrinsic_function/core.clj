(ns intrinsic-function.core
  (:gen-class)
  (:require [clojure.spec.alpha            :as    s       ]
            [clojure.pprint                :refer [pprint]]
            [clojure.set                   :as    set]
            #_[clojure.spec.gen.alpha        :as    gen     ]
            #_[clojure.spec.test.alpha       :as    stest   ]
            #_[clojure.test.check.generators :as    tgen    ]
            #_[clojure.set                   :as    set     ]
            #_[pathetic.core                 :as    path    ]
            #_[asr.lpython                   :as    lpython]))


;;  _  _                                  _               _
;; | \| |__ _ _ __  ___ ___  _ __ _ _ ___| |_ ___  __ ___| |
;; | .` / _` | '  \/ -_|_-< | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;; |_|\_\__,_|_|_|_\___/__/ | .__/_| \___/\__\___/\__\___/_|
;;                          |_|


(defprotocol Names
  (free-names  [this])
  (bound-names [this])
  (recursor    [this bound-or-free]))


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


(defn flatten- [kit]
  (-> kit
      par->vec
      repars))


;;  _   _ _   _                      _         _
;; | |_(_) |_| |_ ___ _ _    __ __ _| |__ _  _| |_  _ ___
;; | / / |  _|  _/ -_) ' \  / _/ _` | / _| || | | || (_-<
;; |_\_\_|\__|\__\___|_||_| \__\__,_|_\__|\_,_|_|\_,_/__/


;; -+-+-+-
;;  n a p
;; -+-+-+-


(defrecord nap     []

  Names  (free-names  [_] #{})  (bound-names [_] #{})

  Flatten  (par->vec [this]    this)
  (repars   [this]    this)

  Subst

  (subst    [this _ _] this)
  (patch-up [this _]   this))


;; -+-+-+-+-
;;  p a r s
;; -+-+-+-+-


(defrecord pars    [kits]

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


(defrecord par     [K L]

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


;; Apparently need the following witnesses for org-babel
;; (org-latex-export-to-pdf doesn't work well without them).


(def kit-1
  (say. 'x 'z (nap.)))
        ;; => {}))


(def kit-2
  (hear. 'x 'y
         (say. 'y 'x
               (hear. 'x 'y (nap.)))))
  ;; => {:chan x, :msg y, :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}})


(def kit-3
  (hear. 'z 'v
         (say. 'v 'v (nap.))))
  ;; => {:chan z, :msg v, :K {:chan v, :msg v, :K {}}})


(def whisper-boat
  (channel. 'x
            (par. kit-1
                  (par. kit-2 kit-3))))
  ;; => {:x x,
  ;;     :K
  ;;     {:K {:chan x, :msg z, :K {}},
  ;;      :L
  ;;      {:K {:chan x, :msg y, :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}},
  ;;       :L {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}}}})


;; UNEXPLAINED: "pars." syntax does not work in test,
;;              but it does work in core. In test, it throws
;;              a compile-time IllegalArgumentException.
(def whisper-boat-2
  (channel. 'x
            (pars. [kit-1 kit-2 kit-3])))
  ;; => {:x x,
  ;;     :K
  ;;     {:kits
  ;;      [{:chan x, :msg z, :K {}}
  ;;       {:chan x, :msg y, :K {:chan y, :msg x, :K {:chan x, :msg y, :K {}}}}
  ;;       {:chan z, :msg v, :K {:chan v, :msg v, :K {}}}]}})


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


;;  ___        _         _   _
;; | _ \___ __| |_  _ __| |_(_)___ _ _
;; |   / -_) _` | || / _|  _| / _ \ ' \
;; |_|_\___\__,_|\_,_\__|\__|_\___/_||_|


;; -+-+-+-+-+-+-+-+-
;;  M a t c h i n g
;; -+-+-+-+-+-+-+-+-


(defn find-outermost-par [kit]
  )


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
