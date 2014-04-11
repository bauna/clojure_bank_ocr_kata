(ns bank-ocr.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [java.io LineNumberReader]))

(def ZERO [(seq " _ ")
           (seq "| |")
           (seq "|_|")])

(def ONE [(seq "   ")
          (seq "  |")
          (seq "  |")])

(def TW0 [(seq " _ ")
          (seq " _|")
          (seq "|_ ")])

(def THREE [(seq " _ ")
            (seq " _|")
            (seq " _|")])

(def FOUR [(seq "   ")
           (seq "|_|")
           (seq "  |")])

(def FIVE [(seq " _ ")
           (seq "|_ ")
           (seq " _|")])

(def SIX [(seq " _ ")
          (seq "|_ ")
          (seq "|_|")])

(def SEVEN [(seq " _ ")
            (seq "  |")
            (seq "  |")])

(def EIGHT [(seq " _ ")
            (seq "|_|")
            (seq "|_|")])

(def NINE [(seq " _ ")
           (seq "|_|")
           (seq " _|")])


(def segments-to-int {ZERO 0 ONE 1 TW0 2 THREE 3 FOUR 4 FIVE 5 SIX 6 SEVEN 7 EIGHT 8 NINE 9})

(defn checksum
  [acct-num]
  (zero? (mod (apply + (map * (range 9 0 -1) acct-num)) 11)))

(defn- validate 
  [acct-segments]
  (let [acct-num (map segments-to-int acct-segments)]
    (and (not-any? nil? acct-num) 
         (checksum acct-num))))

(defn gen-fixes
([acct-segments]
  (let [fixes (atom #{})]
    (gen-fixes acct-segments fixes)
    @fixes))
([acct-segments fixes] 
	(doseq [pos [0 1 2 3 4 5 6 7 8]
	        row [0 1 2]
	        col [0 1 2]
	        segment (seq " _|")]
   (let [num (nth acct-segments pos)
         is-ill (not (segments-to-int num)) 
	       new-acct (assoc-in acct-segments [pos row col] segment)]
     (if (validate new-acct) (swap! fixes conj (map segments-to-int new-acct))))))) 
       

(defn- part
  [n l] 
  (map vec (partition n l)))

(defn parse-account
  [handler acct-lines]
  (let [l1 (doall (first acct-lines))
        l2 (doall (second acct-lines))
        l3 (doall (nth acct-lines 2))
        acct-segments (vec (map vector (part 3 l1) (part 3 l2) (part 3 l3)))
        acct-num (map segments-to-int acct-segments)
        is-ill (some nil? acct-num)
        is-err (if-not is-ill (not (checksum acct-num)))]
    (if (or is-err is-ill) 
      (handler acct-num (gen-fixes acct-segments) is-err is-ill)
      (handler acct-num))))

(defn line-seq-4
  [^java.io.BufferedReader rdr]
  (try 
    (when-let [line1 (.readLine rdr)]
      (when-let [line2 (.readLine rdr)]
        (when-let [line3 (.readLine rdr)]
          (when-let [_ (.readLine rdr)]
            (cons [line1 line2 line3] (lazy-seq (line-seq-4 rdr)))))))
    (catch Exception e (.printStackTrace e))))

(defn parse
  "parse bank ocr account format"
  [reader handler]
  (doall (map #(parse-account handler %) (line-seq-4 reader))))
 
(defn- error-string
  [is-err is-ill]
  (if is-ill "ILL" 
    (if is-err "ERR" "")))

(defn- acct-to-string
  [acct-num]
  (s/join (map #(if % % "?") acct-num)))

(defn- fixes-to-string 
  [fixes]
  (str "[" (s/join ", " (map #(str "'" (acct-to-string %)  "'") fixes)) "]"))

(defn parse-to-writer
  [reader writer]
  (let [handler 
        (fn 
          ([acct-num] 
            (.write writer (acct-to-string acct-num))
            (.write writer "\n"))
          ([acct-num fixes is-err is-ill]
            (let [fixes-count (count fixes) ]
              (cond 
                (= 0 fixes-count) (.write writer (str (acct-to-string acct-num) " " (error-string is-err is-ill)))
                (= 1 fixes-count) (.write writer (acct-to-string (first fixes)))
                :else (.write writer (str (acct-to-string acct-num) " AMB " (fixes-to-string fixes)))))
            (.write writer "\n")))]
    (parse reader handler)))
