#lang plait


{letvar x {box 3}
  {letvar y x
  {begin {set-box! y 10}
    {unbox x}}}}

;; initially
;; env empty
;; empty store

;; Interpret {box 3}, gives a new value and state
;; value: (BoxV 0), store is ([0]:=3)

;; Interpret
{letvar y x
  {begin {set-box! y 10}
    {unbox x}}}
;; env x := (BoxV 0),
;; store [0] := 3

;;Interpreting x gives value (BoxV 0) and state ([0]:=3)

{begin {set-box! y 10}
    {unbox x}}
;; env y := (BoxV 0), x := (BoxV 0)
;; store [0] := 3

;; {set-box! y 10}
;; evaluates y to (BoxV 0)
;; evaluates 10 to 10
;; returns 10, and store with 10 at y's location, e.g. 0

{unbox x}
;; env y := (BoxV 0), x := (BoxV 0)
;; store [0] := 10

;; x evaluates to (BoxV 0)
;; fetch value from location 0, gives 10
;; result of unbox is 10
;; Result of {unbox x} is result of the whole thing
