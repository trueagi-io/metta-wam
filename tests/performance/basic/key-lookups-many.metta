; Initialize data space
!(bind! &data (new-space))

; Define new-entry! function
(= (new-entry! $key $value)
   (let $new-state (new-state $value)
        (add-atom &data (= (get-data $key) $new-state))
    )
)

; Populate the data space with entries manually for size 10
!(new-entry! k-0 v-0)
!(new-entry! k-1 v-1)
!(new-entry! k-2 v-2)
!(new-entry! k-3 v-3)
!(new-entry! k-4 v-4)
!(new-entry! k-5 v-5)
!(new-entry! "k-6" v-6)
!(new-entry! k-7 v-7)
!(new-entry! k-8 v-8)
!(new-entry! k-9 v-9)

!(assertEqual
   (let $v (new-state v-0)
    (match &data (= (get-data $k) $v) $k))
   k-0)

!(assertEqual
   (let $v (new-state v-1)
    (match &data (= (get-data $k) $v) $k))
   k-1)

!(assertEqual
   (let $v (new-state v-2)
    (match &data (= (get-data $k) $v) $k))
   k-2)

!(assertEqual
   (let $v (new-state v-3)
    (match &data (= (get-data $k) $v) $k))
   k-3)

!(assertEqual
   (let $v (new-state v-4)
    (match &data (= (get-data $k) $v) $k))
   k-4)

!(assertEqual
   (let $v (new-state v-5)
    (match &data (= (get-data $k) $v) $k))
   k-5)

!(assertEqual
   (let $v (new-state v-6)
    (match &data (= (get-data $k) $v) $k))
   "k-6")

!(assertEqual
   (let $v (new-state v-7)
    (match &data (= (get-data $k) $v) $k))
   k-7)

!(assertEqual
   (let $v (new-state v-8)
    (match &data (= (get-data $k) $v) $k))
   k-8)

!(assertEqual
   (let $v (new-state v-9)
    (match &data (= (get-data $k) $v) $k))
   k-9)

!(assertEqual
   (let $k k-1
     (match &data (= (get-data $k) $v) (get-state $v)))
   v-1)


!(assertEqual (get-state (match &data (= (get-data k-7) $v) $v))
   v-7)

!(assertEqual (match &data (= (get-data k-7) $v) (get-state $v))
   v-7)

