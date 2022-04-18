module Gitem

open System

[<CustomComparison>]
[<CustomEquality>]
type Gitem =
    struct
        val mutable public Ri: int
        val mutable public Pi: int
        val mutable public La: string
        val mutable public Processed: bool

        new(ri: int, pi: int, la: string) = { Ri = ri; Pi = pi; La = la; Processed = false }

        interface IComparable<Gitem> with
            member this.CompareTo obj =
                if obj = Unchecked.defaultof<_> then 1
                else 
                    let mutable expr = (this.Ri * 65536/2 + this.Pi) - (obj.Ri * 65536/2 + obj.Pi)
                    if expr = 0 then
                        expr <- this.La.CompareTo(obj.La)
                    expr
        
        interface IComparable with
            member this.CompareTo obj = 
                match obj with
                | null ->  1
                | _ ->
                    let other = obj :?> Gitem
                    if not (other = Unchecked.defaultof<_>) then
                        let mutable expr = (this.Ri * 65536/2 + this.Pi) - (other.Ri * 65536/2 + other.Pi)
                        if expr = 0 then
                            expr <- this.La.CompareTo(other.La)
                        expr
                    else 1

        interface IEquatable<Gitem> with
            member this.Equals obj = 
                this.Ri = obj.Ri && this.Pi = obj.Pi && this.La = obj.La
        
        override this.Equals obj =
            let other = obj :?> Gitem
            if not(other = Unchecked.defaultof<_>) then
                this.Ri = other.Ri && this.Pi = other.Pi && this.La = other.La
            else
                false

        override this.GetHashCode () = 
            ("" + this.Ri.ToString() + "," + this.Pi.ToString() + "," + this.La).GetHashCode()

        override this.ToString() = 
            "Gitem::  Rule Index: " + this.Ri.ToString() + " :: Dot: " + this.Pi.ToString() + " :: lookahead: " + this.La;
    end