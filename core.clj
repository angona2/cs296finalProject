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
           :lock 1}
   :Sewer {:desc "Dark and smelly in here... Oh god there are Rats! "
           :title "in the sewer"
           :dir {:north :Base,
                 :south :Stable}
           :contents "Rat"
           :val 0
           :lock 1}
   :Final {:desc "The final room of the game... Here we go! "
           :title "in the final room"
           :dir {:west :Base}
           :contents "Book"
           :val 0
           :lock 0}
   :Storage {:desc "The storage room, looks like a bunch of random stuff! "
             :title "in the storage room"
             :dir {:east :Base,
                   :south :Armory,
                   :north :Training,
                   :west :Closet}
             :contents "Broom"
             :val 0
             :lock 1}
     :Lab {:desc "Room for preparing chemicals to test hypothesis. "
           :title "in the lab"
           :dir {:south :Base,
                 :east :ExperimentRm,
                 :west :Kitchen}
           :contents "Vial"
           :val 1
           :lock 1}
   :ExperimentRm {:desc "Room that allows for testing chemicals. "
                  :title "in the experiment room"
                  :dir {:west :Lab}
                  :contents "Potion"
                  :val 0
                  :lock 1}
   :Armory {:desc "Area containing locked lockers with weapons and armory, what a tease. "
            :title "in the armory"
            :dir {:north :Storage,
                  :west :Wardrobe}
            :contents "Knife"
            :val 1
            :lock 1}
   :Training {:desc "Lots of dummys and targets for practice. "
              :title "in the training room"
              :dir {:south :Storage}
              :contents "Dummy"
              :val 1
              :lock 1}
   :Closet {:desc "This room appears to be a closet with... OMG THERE IS A DEAD BODY IN HERE! "
            :title "in the closet room"
            :dir {:east :Storage}
            :contents "Body"
            :val 0
            :lock 1}
   :Stable {:desc "An old horse stable without horses, there is computer though. (Try 'use')"
            :title "in the stable"
            :dir {:north :Sewer}
            :contents "Hay"
            :val 1
            :lock 1}
   :Wardrobe {:desc "This room contains lots of clothes. "
              :title "in the wardrobe"
              :dir {:east :Armory,
                    :south :Bathroom}
              :contents "Shirt"
              :val 0
              :lock 1}
   :Bathroom {:desc "This is a bathroom. "
              :title "in the bathroom"
              :dir {:north :Wardrobe}
              :contents "toiletpaper"
              :val 1
              :lock 1}
   :Kitchen {:desc "Yes! This is a kitchen and there is pizza! Wait who else is here... "
             :title "in the kitchen"
             :dir {:east :Lab,
                   :south :Barracks}
             :contents "Pizza"
             :val 1
             :lock 1}
   :Barracks {:desc "Sleeping quarters for people in this Base. "
              :title "in the Barracks"
              :dir {:north :Kitchen,
                    :east  :Library}
              :contents "Blanket"
              :val 0
              :lock 1}
   :Library {:desc "This room contains lots of old books. "
             :title "in the Library"
             :dir {:west :Barracks}
             :contents "Old Book"
             :val 1
             :lock 1}})

                      
(def player
  {:location :Base
   :inventory #{}
   :locked 0
   :puzVal 0
   :hp 10
   :seen #{}})

(defn status [player]
  (let [location (player :location)
        val (get-in the-map [location :val])
        lock (get-in the-map [location :lock])]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn search [player]
  (let [location (player :location)
        inv (get-in player [:inventory])
        content (get-in the-map [location :contents])]
    (if(contains? inv content)
      (do (println (str "\nYou have already picked up " (-> the-map location :contents) ".\n")) player)
      (do (println (str "\nThis room contains " (-> the-map location :contents) ".\n")) player))))
      
(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)
        lock (get-in the-map [dest :lock])
        locked (player :locked)
        inv  (get-in player [:inventory])]
       
      (if (nil? dest)
        (do (println "You can't go that way guy. Sorry buddy")
            player)
        (if(= lock 0)
          (if(= locked 0)
            (do (println "This door is locked sorry.") player)
            (assoc-in player [:location] dest))
          (if(contains? inv "Table")
            (do (println "Your character is too heavy to move.") player)
            (assoc-in player [:location] dest))))))
           

(defn tock [player]
  (let [val (get-in player [:puzVal])]
   (if(>= val 4)
     (do (println (str "\nYou have succeeded. Check your value ('checkVal') for reward.\n")) (assoc-in player [:location] :Base))
     (do (println (str "\nYou have been teleported back to Base.\n")) (assoc-in (update-in player [:puzVal] inc) [:location] :Base)))))
   
(defn teleport [loc player]
  (let [currLoc (player :location)
        currVal (get-in the-map [currLoc :val])]
      (if(and(not= currLoc loc)(= currVal 0))
        (assoc-in player [:location] loc)
        (if(= currLoc loc)
          (do (println "You are already at the base.") player)
          (tock player)))))
                     
(defn pickup [player]
  (let [inventory (get-in player [:inventory])
        currRoom  (get-in player [:location])
        contents  (get-in the-map [currRoom :contents])]
    (if(contains? inventory contents)
      (do (println "\nYour inventory already contains this item.\n")
        player)
      (do (println (str "\nYou have picked up a " (-> the-map currRoom :contents) ".\n")) (update-in player [:inventory] #(conj % contents))))))

(defn drop [player]
  (let [inventory (get-in player [:inventory])
        currLoc   (get-in player [:location])
        location  (get-in the-map [currLoc :contents])]
    (if(contains? inventory "Table")
     (do(println(str "\nYou dropped the Table, silly to pick up anyways.\n")) (update-in player [:inventory] #(disj % "Table")))
     (do(println "\nYou do not have anything heavy that needs to be dropped.\n")player))))
      
(defn inv [player]
  (do (println (seq (player :inventory)))
    player))

(defn checkVal [player]
  (let [val (get-in player [:puzVal])
        inv (get-in player [:inventory])
        loc (player :location)]
    (if(< val 4)
      (do (println (str "\nCurrent val at " (-> player :puzVal) ". Need more to recieve prize.\n"))player)
      (do(println(str "\nYou're teleport strategy has reached 4. Key added to inv.\n")) (update-in player [:inventory] #(conj % "Key"))))))

(defn unlock [player]
  (let [inv (get-in player [:inventory])
        loc (player [:location])
        final (get-in the-map [:Final])]
    (if(contains? inv "Key")
      (do (println (str "\nYou have unlocked the door!\n")) (update-in player [:locked] inc))
      (do (println "\nYou do not have a Key yet.\n") player))))
    
(defn read [player]
  (let [inv (get-in player [:inventory])]
    (if(contains? inv "Old Book")
      (do (println "\nWorkers Log\nDay 1: Looking forward to working in this high tech lab\nDay 2: We are working on a potential break through, and I will leave it at that\nDay 3: People are starting to get sick. Not looking good.\nDay 4: Dear god... I need to get the hell out of here... (end of log)\n") player)        
      (if(contains? inv "Book")
       (do (println "\n39210849032014893920148932. Take this to the stables and input in the computer.\nGet to the stables through the Sewer.\n") player) 
       (do (println "\nYou have no book to read.\n") player)))))

(defn use [player]
  (let [inv (get-in player [:inventory])
        currRoom (player [:location])
        stable (get-in the-map [currRoom :contents])]
    (if(contains? inv "Book")
      (do (println "Computer: Code Accepted. Player being ported to safety.\nCongrats you've won!!") (System/exit 0)) 
      (do (println "You have nothing to use.") player))))

(defn consume [player]
  (let [inv (get-in player [:inventory])]
    (if(contains? inv "Potion")
      (do (println (str "You have consumed a Potion, your stomach burns.")) (update-in (update-in player [:inventory] #(disj % "Potion")) [:hp] - 5))
      (if(contains? inv "Body")
        (do (println (str "You have consumed a... Body?!?! Sickness granted. Player dead. Game over.")) (System/exit 0))
        (if (contains? inv "Pizza")
          (do (println (str "Mmmmm boopity boppity's famous pizza, I feel my health increasing.")) (update-in (update-in player [:inventory] #(disj % "Pizza")) [:hp] + 5))
          (if (contains? inv "Rats")
            (do (println (str "Eating a rat seems gross but okay here it goes... Fine I guess.")) (update-in (update-in player [:inventory] #(disj % "Rats")) [:hp] + 1))
            (do (println "You have nothing consumable.") player)))))))

(defn checkHealth [player]
  (let [hp (player [:hp])]
    (do (println (str "Your current health is " (-> player :hp) ". ")) player)))
    

(def help "\nWelcome to the List of Commands\n\n1. 'north' or 'n' = move player north\n2. 'south or 's' = move player south\n3. 'east' or 'e' = move player east\n4. 'west' or 'w' = move player west\n5. 'search' = displays contents in room\n6. 'look' = Display current room\n7. 'grab' = Pick up contents in room\n8. 'inventory' or 'i' = Displays current inventory\n9. 'drop' = Drops items that weigh too much to carry\n10. 'home' = Teleports you back to base\n11. 'checkVal' = Checks your Teleportation Value\n12. 'unlock' = Attempts to unlock a locked door \n13. 'read' = Allows user to read book if picked up \n14. 'use' = Allows user to use specific item(s) \n15. 'consume' = Eats anything consumable in inventory\n16. 'checkHealth' = Displays current health\n17. 'help' = Displays list of commands\n")

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
         [:drop]           (drop player)
         [:checkVal]       (checkVal player)
         [:unlock]         (unlock player)
         [:read]           (read player)
         [:use]            (use player)
         [:consume]        (consume player)
         [:checkHealth]    (checkHealth player)
    
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
