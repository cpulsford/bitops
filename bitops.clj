;;
;; A library of useful low-level binary operations.
;; Cameron Pulsford
;;

(ns binary)

;;
;; Integer (whole number) specific bit operations.
;;

(defprotocol IntegerBitOps
  (bit-rotate-left [x n] "Bitwise rotate left.")
  (bit-rotate-right [x n] "Bitwise rotate right."))

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
  (to-binary [x] "Returns the 2's complement binary string representation of arg.")
  (to-base [x radix] "Returns the string representation of arg in the given radix. Where it would apply, results are not sign extended.")
  (bit-count [x] [x radix] "Returns the number of digits to represent arg in the given radix. If no radix is supplied, results are sign extended (where makes sense) and 2 is the default."))

(extend Integer
  GeneralBitOps
  {:to-binary (fn [x] (Integer/toBinaryString x))
   :to-base (fn [x radix] (Integer/toString x radix))
   :bit-count (fn ([x] (- 32 (Integer/numberOfLeadingZeros x)))
                  ([x radix] (-> (Math/abs x) (to-base radix) count)))})

;; Returns a common implementation of bit-count.
(defn- b-count [] {:bit-count (fn ([x] (-> x to-binary count))
                                  ([x radix] (-> (Math/abs x) (to-base radix) count)))})

(extend Long
  GeneralBitOps
  (merge (b-count)
         {:to-binary (fn [x] (Long/toBinaryString x))
          :to-base (fn [x radix] (Long/toString x radix))}))

(extend BigInteger
  GeneralBitOps
  (merge (b-count)
         {:to-binary (fn [x] (.toString x 2))
          :to-base (fn [x radix] (.toString x radix))}))

(extend Float
  GeneralBitOps
  (merge (b-count)
         {:to-binary (fn [x] (-> x Float/floatToIntBits Integer/toBinaryString))
          :to-base (fn [x radix] (-> x Float/floatToIntBits (Integer/toString radix)))}))

(extend Double
  GeneralBitOps
  (merge (b-count)
         {:to-binary (fn [x] (-> x Double/doubleToLongBits Long/toBinaryString))
          :to-base (fn [x radix] (-> x Double/doubleToLongBits (Long/toString radix)))}))

;;
;; Type agnostic bit functions.
;;

(defn mask
  "Returns a number that represents a mask of the given length.
   Optionally, a (left) shift amount may be supplied."
  ([n]
   (-> (bit-shift-left 1 n) dec))
  ([n shift]
   (-> (bit-shift-left 1 n) dec (bit-shift-left shift))))

(defn isolate-range
  "Returns the number represented by isolating arg at the given bit.
   If a length to isolate by is not given, the rest of the bit string
   will be used."
  ([x n]
   (-> (bit-count x) (- n) inc (mask n) (bit-and x) (bit-shift-right n)))
  ([x n len]
   (-> (mask len n) (bit-and x) (bit-shift-right n))))

(defn compose
  "Compose x into the base number at the given bit. If a length is
   given, an exception will be thrown if the bit-count of x is
   greater than length."
  ([base x n]
   (-> (bit-count x) (mask n) bit-not (bit-and base) (bit-or (bit-shift-left x n))))
  ([base x n len]
   {:pre [(<= (bit-count x) len)]}
   (compose base x n)))

;;
;; The following functions have the same semantics as their Java counterparts.
;;

(defn << 
  "Bitwise arithmetic left shift."
  [x n]
  (bit-shift-left x n))

(defn >>
  "Bitwise arithmetic right shift."
  [x n]
  (bit-shift-right x n))

(defn >>>
  "Bitwise logical right shift."
  [x n]
  (isolate-range x n (- (bit-count x) n)))
