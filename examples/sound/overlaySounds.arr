import global as G
import sound as S
import list as L

#Getting sound object from URL
#Output is a widget through which sound can be played, paused or downloaded
soundA = S.getSoundFromURL('http://bbcsfx.acropolis.org.uk/assets/07075055.wav')
soundB = S.getSoundFromURL('http://bbcsfx.acropolis.org.uk/assets/07075065.wav')


soundList = [L.list: soundA, soundB]

#Output is a widget through which overlayed sound can be played, paused or downloaded
r = S.overlay(soundList)







