(ns dpos.delegation
  (:require [com.rpl.specter :refer :all]))



;; approval voting

;; - delegate membership updates:
;;   + delegate adding tx costs some fee
;;   + voting tx costs some fee
;;   + approval voting
;;   + need some statistics about nodes to allow community control,
;;     e.g. amounts of blocks dropped, can be part of chain
;;   + update connections after each received block
;;   + transitive voting?
;;   + self voting?



(def registry {:peerA {:balance 5000
                       :approvals #{:peerB}
                       :address "wss://meta.serv1.us:38237"}
               :peerB {:balance 2000
                       :approvals #{:peerC}
                       :address "wss://meta.meinserver.de:3333"}
               :peerC {:balance 3000
                       :approvals #{:peerA :peerB}
                       :address "wss://meta.host.ru:38237"}})


(defn approval-voting [registry]
  (-> (select [MAP-VALS :approvals ALL] registry)
     frequencies))


(approval-voting registry)

(defn weighted-approval-voting [registry]
  (->> (vals registry)
     (reduce (fn [acc e]
               (reduce #(update %1 %2 (fnil + 0) (:balance e))
                       acc
                       (seq (:approvals e))))
             {})))


(weighted-approval-voting registry)


(defn top [registry n]
  (->> (weighted-approval-voting registry)
     (sort-by val)
     reverse
     (take n)
     (into {})))

(top registry 2)





;; 200 genesis-stakeholders {:a 1 :b 9 :c 3....}
;; 31 delegates
;; initial vote  200 => 31


; how to arrivate at the list of delegates? => "registration"

;[{:parent nil, :height 0, :txs []} {:txs [0], :height 1, :parent #uuid "201986f3-84c2-5bd4-b296-508bf65995e5"} {:txs [1], :height 2, :parent #uuid "3e8e2bea-7694-5024-9b91-ebd4b4684a21"} {:txs [2], :height 3, :parent #uuid "13df1615-229a-5412-a6b2-7923451f653d"} {:txs [3], :height 4, :parent #uuid "16254acc-e951-5ea1-a6b6-c6b7393ad1fc"} {:txs [4], :height 5, :parent #uuid "016957ae-c937-5c9a-ba19-7fd6cda6ba3c"} {:txs [5], :height 6, :parent #uuid "238e5de3-6348-5114-a346-995f7f147554"} {:txs [6], :height 7, :parent #uuid "07668d7e-27f5-5725-b75e-701ad8b42e01"} {:txs [7], :height 8, :parent #uuid "2b3d6c69-b090-5d88-9b1e-b5b6c9157e36"} {:txs [8], :height 9, :parent #uuid "1ea6bee1-c0b9-56cd-8039-fe0502d90058"} {:txs [9], :height 10, :parent #uuid "31161db7-4d25-5eea-95a3-381e313aec83"}]
