(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:Base {:desc "Looks like the center of a text based game. "
           :title "in the base"
           :dir {:south :Sewer,
                 :east  :Final,
                 :west  :Storage,
                 :north :Lab}
           :contents "Table"
           :val 0
           :lock 0}
   :Sewer {:desc "Dark and smelly in here... Oh god there are Rats! "
           :title "in the sewer"
           :dir {:north :Base,
                 :south :Stable}
           :contents "Rats"
           :val 1
           :lock 0}
   :Final {:desc "The final room of the game... Here we go! "
           :title "in the final room"
           :dir {:west :Base}
           :contents "Book"
           :val 0
           :lock 1}
   :Storage {:desc "The storage room, looks like a bunch of random stuff! "
             :title "in the storage room"
             :dir {:east :Base,}
                 :south :Armory,
                 :north :Training,
                 :west :Closet
             :contents "Broom"
             :val -1
             :lock 0}
     :Lab {:desc "Room for preparing chemicals to test hypothesis. "
           :title "in the lab"
           :dir {:south :Base,
                 :east :ExperimentRm
                 :west :Kitchen}
           :contents "Vial"
           :val 1
           :lock 0}
   :ExperimentRm {:desc "Room that allows for testing chemicals. "
                  :title "in the experiment room"
                  :dir {:west :Lab}
                  :contents "Potion"
                  :val 1
                  :lock 0}
   :Armory {:desc "Area containing locked lockers with weapons and armory, what a tease. "
            :title "in the armory"
            :dir {:north :Storage,
                  :west :Wardrobe}
            :contents "Knife"
            :val 1
            :lock 0}
   :Training {:desc "Lots of dummys and targets for practice. "
              :title "in the training room"
              :dir {:south :Storage}
              :contents "Dummy"
              :val 1
              :lock 0}
   :Closet {:desc "This room appears to be a closet with... OMG THERE IS A DEAD BODY IN HERE! "
            :title "in the closet room"
            :dir {:east :Storage}
            :contents "Body"
            :val 1
            :lock 0}
   :Stable {:desc "The sewer tunnel led outside to a horse stable. "
            :title "in the stable"
            :dir {:north :Sewer}
            :contents "Hay"
            :val 1
            :lock 0}
   :Wardrobe {:desc "This room contains lots of clothes. "
              :title "in the wardrobe"
              :dir {:east :Armory,
                    :south :Bathroom}
              :contents "Shirt"
              :val 1
              :lock 0}
   :Bathroom {:desc "This is a bathroom. "
              :title "in the bathroom"
              :dir {:north :Wardrobe}
              :contents "toiletpaper"
              :val 1
              :lock 0}
   :Kitchen {:desc "Yes! This is a kitchen and there is pizza! Wait who else is here... "
             :title "in the kitchen"
             :dir {:east :Lab,}
                  :south :Barracks
             :contents "Pizza"
             :val 1
             :lock 0}
   :Barracks {:desc "Sleeping quarters for people in this Base. "
              :title "in the Barracks"
              :dir {:north :Kitchen,
                    :east  :Library}
              :contents "Blanket"
              :val 1
              :lock 0}
   :Library {:desc "This room contains lots of old books. "
             :title "in the Library"
             :dir {:west :Barracks}
             :contents "old Book"
             :val 1
             :lock 0}})

                      
(def player
  {:location :Base
   :inventory #{}
   :tick 0
   :puzVal 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn search [player]
  (let [location (player :location)]
    (do (println (str "This room contains " (-> the-map location :contents) ". "))player)))
        

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)
        valAssociated (player :location)
        lock (-> the-map location :lock)]
      (if (= valAssociated 1)
        (do (update-in player [:val] inc)))
      (if (nil? dest)
        (do (println "You can't go that way guy. Sorry buddy")
            player)
        (assoc-in player [:location] dest))))
     

(defn tock [player]
  (update-in player [:tick] inc))

(defn teleport [loc player]
  (let [puzzleVal (player :puzVal)
        currLoc (player :location)]
      (if(= currLoc loc)
        (do (println "You are already in the Base.")player)
          
        (assoc-in player [:location] loc))))
        

(defn pickup [player]
  (let [inventory (get-in player [:inventory])
        currRoom  (get-in player [:location])
        contents  (get-in the-map [currRoom :contents])]
    (if(contains? inventory contents)
      (do (println "You're inventory already contains this item.")
        player)
      (update-in player [:inventory] #(conj % contents)))))

(defn inv [player]
  (do (println (seq (player :inventory)))
    player))
  
(def help "\nWelcome to the List of Commands\n\n1. 'north' or 'n' = move player north\n2. 'south or 's' = move player south\n3. 'east' or 'e' = move player east\n4. 'west' or 'w' = move player west\n5. 'search' = displays contents in room\n")

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         [(:or :n :north)] (go :north player)
         [(:or :s :south)] (go :south player)
         [(:or :e :east)]  (go :east player)
         [(:or :w :west)]  (go :west player)
         [:search]         (search player)
         [:help]           (do (println help) player)
         [:home]           (teleport :Base player)
         [:grab]           (pickup player)
         [(:or :i :inventory)] (inv player)
       
         _ (do (println (str "That is not on the list, please refer to '/help'.")) player)))   
          

(defn -main

  [& args]
  (println "Welcome to the crappy text game! Type '/help' for list of commands.\n\n")
  (loop [local-map the-map
         local-player player]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
