module UITypes
    ( UIWFF
    , UISequent
    , UIProof
    , UIDeduction
    ) where

import WFF (WFF)
import Sequent (Sequent)
import Proof (Proof, Deduction)
import Lemmon (LemmonRule)

type UIWFF = WFF String String String
type UISequent = Sequent String String String
type UIProof = Proof LemmonRule UIWFF String
type UIDeduction = Deduction LemmonRule UIWFF
