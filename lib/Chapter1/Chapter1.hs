module Chapter1.Chapter1 where

import Euterpea
import Data.Ratio

concertA, a440 :: (PitchClass, Octave)
concertA = (A,4)
a440 = (A, 4)

-- | (:=:) -> Sequential composition (of notes)
--   (:+:) -> Parallel composition (of notes)
--   (trans) -> Raises/Lowers a pitch p by i semitones
hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0
hList d (p:ps) = hNote d p :=: hList d ps
