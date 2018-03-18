(ns dpos.core
  (:require [com.rpl.specter :refer :all]
            [hasch.core :refer [uuid]]))


;; simple DPoS


(def quorum (/ 2 3))


(def genesis {:parent nil
              :pos 0
              :txs []})



(def init
  {:peerA {:head (uuid genesis)
           :id :peerA
           :pending []
           :blocks {(uuid genesis) genesis}}
   :peerB {:head (uuid genesis)
           :id :peerB
           :pending []
           :blocks {(uuid genesis) genesis}}
   :peerC {:head (uuid genesis)
           :id :peerC
           :pending []
           :blocks {(uuid genesis) genesis}}})



;; model sequence of steps
;; in each step peer is selected

(defn send-transaction [state peer-id tx]
  (update-in state [peer-id :pending] conj tx))



(defn create-block [peer]
  (let [{:keys [chain pending id blocks]} peer
        max-block (apply max-key :pos (vals blocks))
        new-block {:txs pending
                   :pos (inc (:pos max-block))
                   :parent (uuid max-block)}]
    {:head new-block
     :pending []
     :id id
     :blocks (assoc blocks (uuid new-block) new-block)}))



(defn send-new-block [state peer]
  (let [block (:head (state peer))]
    (transform [MAP-VALS #(not= peer (:id %)) :blocks]
               #(assoc % (uuid block) block)
               state)))


(comment
  (send-new-block init :peerA))



(defn find-longest-chain [peer]
  (let [{:keys [blocks]} peer
        m (apply max-key :pos (vals blocks))]
    (loop [chain []
           b m]
      (if b
        (recur (conj chain b) (blocks (:parent b)))
        (vec (reverse chain))))))


(defn settled-chain [chain peers]
  (let [c (count peers)]
    (drop-last (Math/ceil (* quorum c)) chain)))



;; stateful simulation:


(def state (atom init))


(clojure.pprint/pprint @state)

(defn peer-order [peers steps]
  (->> peers 
       cycle
       (take steps)
       #_shuffle
       vec))


;; perfect simulation
(defn simulate [steps]
  (let [block-order (peer-order (keys init) steps)]
    (for [i (range steps)]
      (swap! state
             (fn [old]
               (let [peer (block-order i)]
                 (-> old
                    ;; 1. collect transactions for block
                    (send-transaction peer i)
                    ;; 2. create a block
                    (update peer create-block)
                    ;; 3. send out the block to other peers
                    ;; TODO model indirection through inbox
                    (send-new-block peer))))))))


(simulate 10)

(def longest (find-longest-chain (:peerA @state)))

(def settled (settled-chain longest (keys init)))



(reduce + (mapcat :txs longest)) ;; => 45


;; EOS ?

;; TODO mark settled blocks and ensure longest chain contains all settled blocks
;; simulate discretized time with stochasticity

;; open questions

;; - how is time and peer<->block sequence agreed upon?
;; - model double spending attack

;; orthogonal design questions
;; - collect fees due to tx type (and cost)
;; - ensure public key integrity on system entry
