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
  (bound-names [this]))


;;  ___      _       _                  _               _
;; / __|_  _| |__ __| |_   _ __ _ _ ___| |_ ___  __ ___| |
;; \__ \ || | '_ (_-<  _| | '_ \ '_/ _ \  _/ _ \/ _/ _ \ |
;; |___/\_,_|_.__/__/\__| | .__/_| \___/\__\___/\__\___/_|
;;                        |_|


(defprotocol Subst
  (patch-up [this x])
  (subst    [this x y]))


;;  _   _          _   _ _   _
;; | |_| |_  ___  | |_(_) |_| |_ ___ _ _  ___
;; |  _| ' \/ -_) | / / |  _|  _/ -_) ' \(_-<
;;  \__|_||_\___| |_\_\_|\__|\__\___|_||_/__/


(defrecord nap     []
  Names
  (free-names  [_] #{})
  (bound-names [_] #{})
  Subst
  (subst    [this _ _] this)
  (patch-up [this _]   this))


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
  Subst
  (patch-up [this x]))


(defrecord say     [chan msg K]
  Names
  (free-names [_]
    (set/union
     (set [chan msg])
     (free-names K)))
  (bound-names [_]
    (bound-names K)))


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
  )


(defrecord channel [x K]  ; like nu in the pi calculus
  Names
  (free-names [_]
    (set/difference
     (free-names K)
     #{x}))
  (bound-names [_]
    (set/union
     #{x}
     (bound-names K))))


(defrecord repeat- [K]  ; without hyphen, collides with built-in "repeat"
  Names
  (free-names [_]
    (free-names K))
  (bound-names [_]
    (bound-names K)))


(def kit-1
  (say. 'x 'z (nap.)))


(def kit-2
  (hear. 'x 'y
         (say. 'y 'x
               (hear. 'x 'y (nap.)))))


(bound-names kit-2)  ; #{y}


(def kit-3
  (hear. 'z 'v
         (say. 'v 'v (nap.))))


(def whisper-boat
  (channel. 'x
            (par. kit-1
                  (par. kit-2 kit-3))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (pprint (ns-publics 'intrinsic-function.core))
  (println "Hello, World!"))
