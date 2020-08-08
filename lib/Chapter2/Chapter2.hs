module Chapter2.Chapter2 where

import Euterpea


hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0
hList d (p:ps) = hNote d p :+: hList d ps

t251 :: Music Pitch
t251 = dMinor :+: gMajor :+: cMajor
       where dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
             gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
             cMajor = c 4 bn :=: e 4 bn :=: g 4 bn

t251' :: Music Pitch
t251' = dMinor :+: gMajor :+: cMajor
       where dMinor = d 4 qn :=: f 4 qn :=: a 4 qn
             gMajor = g 4 qn :=: b 4 qn :=: d 5 qn
             cMajor = c 4 qn :=: e 4 qn :=: g 4 qn

-- | Exercise 2.1
twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p dur = chord1  :+: chord2 :+: chord3
                   where chord1 = note dur (trans 2 p) :=: note dur (trans 5 p) :=: note dur (trans 9 p)
                         chord2 = note dur (trans 19 p)  :=: note dur (trans (11) p) :=: note dur (trans 0 p)
                         chord3 = note (dur*2) (trans 0 p) :=: note (dur*2) (trans 4 p) :=: note (dur*2) (trans 7 p)


mel :: [Pitch]
mel = [(C, 4), (G, 4), (A,4), (C,4)]
