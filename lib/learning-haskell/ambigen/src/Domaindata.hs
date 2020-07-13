module Domaindata (
) where 

import Domain 


{-| 
 - Data pertaining to the automated music generation domain.
 - @author A.E.Veltstra
 - @since 2020-06-16T19:35:00.000EDT
 - @version 2020-07-12T00:25:00.000EDT
 -}

bassDrumBeat :: Signal 
bassDrumBeat = Riff 1
                       [
                           [
                               B 0 Drumset,
                               B 1 (Pitch 'C' 2), --Bass Drum
                               B 3 F,
                               B 1200 Start,
                               B 2100 Stop
                           ]
                       ]

bassDrum4Beats :: Signal 
bassDrum4Beats = Riff 1
                        [
                            [
                                B 0 F,
                                B 1200 bassDrumBeat,
                                B 2399 Mp,
                                B 2400 bassDrumBeat,
                                B 3599 Mf,
                                B 3600 bassDrumBeat,
                                B 4799 P,
                                B 4800 bassDrumBeat
                            ]
                        ]

drumRiffs = [
                 bassDrumBeat,
                 bassDrum4Beats
             ]

collection = [
               drumRiffs
           ]

 

--------

