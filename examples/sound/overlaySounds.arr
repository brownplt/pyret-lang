import global as G
import sound as S
import list as L

#Getting sound object from URL
#Output is a widget through which sound can be played, paused or downloaded
soundA = S.getSoundFromURL('http://bbcsfx.acropolis.org.uk/assets/07075055.wav')
soundB = S.getSoundFromURL('http://bbcsfx.acropolis.org.uk/assets/07075065.wav')


#overlay multiple sounds in a list
soundC = S.overlay-list([L.list: soundA, soundB, soundA])

#overlay two sounds
soundD = S.overlay(soundA, soundB)









