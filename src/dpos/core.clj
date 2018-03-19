(ns dpos.core
  (:require [com.rpl.specter :refer :all]
            [hasch.core :refer [uuid]]))


;; simple DPoS


(def quorum (/ 2 3))


(def genesis {:parent nil
              :height 0
              :txs []})


(defn init-peer [id]
  {:head (uuid genesis)
           :id id
           :pending []
           :blocks {(uuid genesis) genesis}})

(def initial-state
  {:peerA (init-peer :peerA)
   :peerB (init-peer :peerB)
   :peerC (init-peer :peerC)})


;; model sequence of steps
;; in each step peer is selected

(defn send-transaction [state peer-id tx]
  (update-in state [peer-id :pending] conj tx))



(defn create-block [peer]
  (let [{:keys [chain pending id blocks]} peer
        max-block (apply max-key :height (vals blocks))
        ;new block from pending tx
        ;new block height is +1 from last
        new-block {:txs pending
                   :height (inc (:height max-block))
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
        m (apply max-key :height (vals blocks))]
    (loop [chain []
           b m]
      (if b
        (recur (conj chain b) (blocks (:parent b)))
        (vec (reverse chain))))))


(defn settled-chain [chain peers]
  (let [c (count peers)]
    (drop-last (Math/ceil (* quorum c)) chain)))



;; stateful simulation:


(def state (atom initial-state))

(defn set-genesis []
  (reset! state (atom initial-state)))


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


;(simulate 10)

(def longest (find-longest-chain (:peerA @state)))

(def settled (settled-chain longest (keys init)))

;(reduce + (mapcat :txs longest)) ;; => 45
