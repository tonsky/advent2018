(ns advent2018.timer)

(defn -main
  "Counts passed time. Press Enter to record a result"
  []
  (let [t0 (System/currentTimeMillis)]
    (loop []
      (print "\033[1G") ; go to beginning of line
      (print "\033[0K") ; clear till the end of line
      (let [passed  (- (System/currentTimeMillis) t0)
            mins    (quot passed 60000)
            secs    (-> passed (mod 60000) (quot 1000))
            msecs   (mod passed 1000)]
        (print (format "%d:%02d.%03d" mins secs msecs)))
      (flush)
      (Thread/sleep 277)
      (recur))))