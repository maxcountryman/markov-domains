(defproject markov-domains "0.1.0-SNAPSHOT"
  :description "Finds available domains using Markov chains."
  :url "https://github.com/maxcountryman/markov-domains"
  :jvm-opts ["-Xmx2g" "-server"] 
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.3.0"]]
  :main markov-domains.core)
