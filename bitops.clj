;;
;; A library of useful low-level binary operations.
;; Cameron Pulsford
;;

(ns binary)

;; Protocols

;;
;; Integer (whole number) specific bit operations.
;;

(defprotocol IntegerBitOps
  (bit-rotate-left [x n])
  (bit-rotate-right [x n]))

(extend-protocol IntegerBitOps
  Integer
  (bit-rotate-left [x n] (Integer/rotateLeft x n))
  (bit-rotate-right [x n] (Integer/rotateRight x n))

  Long
  (bit-rotate-left [x n] (Long/rotateLeft x n))
  (bit-rotate-right [x n] (Long/rotateRight x n)))

;;
;; General bit operations.
;;

(defprotocol GeneralBitOps
  (to-binary [x])
  (to-base [x radix])
  (bit-count [x]))

(extend Integer
  GeneralBitOps
  {:to-binary (fn [x] (Integer/toBinaryString x))
   :to-base (fn [x radix] (Integer/toString x radix))
   :bit-count (fn [x] (- 32 (Integer/numberOfLeadingZeros x)))})

(def b-count {:bit-count (fn [x] (count (to-binary x)))})

(extend Long
  GeneralBitOps
  (merge b-count
         {:to-binary (fn [x] (Long/toBinaryString x))
          :to-base (fn [x radix] (Long/toString x radix))}))

(extend BigInteger
  GeneralBitOps
  (merge b-count
         {:to-binary (fn [x] (.toString x 2))
          :to-base (fn [x radix] (.toString x radix))}))

(extend Float
  GeneralBitOps
  (merge b-count
         {:to-binary (fn [x] (-> x Float/floatToIntBits Integer/toBinaryString))
          :to-base (fn [x radix] (-> x Float/floatToIntBits (Integer/toString radix)))}))

(extend Double
  GeneralBitOps
  (merge b-count
         {:to-binary (fn [x] (-> x Double/doubleToLongBits Long/toBinaryString))
          :to-base (fn [x radix] (-> x Double/doubleToLongBits (Long/toString radix)))}))

;;
;; Type agnostic bit functions.
;;

(defn mask
  ([n]
   (-> (bit-shift-left 1 n) dec))
  ([n shift]
   (-> (bit-shift-left 1 n) dec (bit-shift-left shift))))

(defn isolate-range
  ([x n]
   (-> (bit-count x) (- n) inc (mask n) (bit-and x) (bit-shift-right n)))
  ([x n rng]
   (-> (mask rng n) (bit-and x) (bit-shift-right n))))

(defn compose
  ([base x n]
   (bit-or
     (-> (bit-count x) (mask n) bit-not (bit-and base))
     (bit-shift-left x n)))
  ([base x n rng]
   {:pre [(<= (bit-count x) rng)]}
   (compose base x n)))

;;
;; The following functions have the same semantics as their Java counterparts.
;;

(defn << 
  [x n]
  (bit-shift-left x n))

(defn >>
  [x n]
  (bit-shift-right x n))

(defn >>>
  [x n]
  (isolate-range x n (- (bit-count x) n)))
