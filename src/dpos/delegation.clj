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



