(clear-all)

(define-model subitize

(sgp :v t)

(sgp :show-focus t 
     :visual-num-finsts 10 
     :visual-finst-span 10)

(chunk-type count count state)
(chunk-type number-fact identity next value)

(add-dm (one isa chunk)(two isa chunk)
        (three isa chunk)(four isa chunk)
        (five isa chunk)(six isa chunk)
        (seven isa chunk)(eight isa chunk)
        (nine isa chunk)(ten isa chunk)
        (zero isa chunk) (eleven isa chunk)
        (start isa chunk)
        (n0 isa number-fact identity zero next one value "zero")
        (n1 isa number-fact identity one next two value "one")
        (n2 isa number-fact identity two next three value "two")
        (n3 isa number-fact identity three next four value "three")
        (n4 isa number-fact identity four next five value "four")
        (n5 isa number-fact identity five next six value "five")
        (n6 isa number-fact identity six next seven value "six")
        (n7 isa number-fact identity seven next eight value "seven")
        (n8 isa number-fact identity eight next nine value "eight")
        (n9 isa number-fact identity nine next ten value "nine")
        (n10 isa number-fact identity ten next eleven value "ten")
        (goal isa count state start)) 


(goal-focus goal)

)
