import global as G
import sound as S

#Getting sound object from URL
#Output is a widget through which sound can be played, paused or downloaded
soundA = S.get-sound-from-url('http://bbcsfx.acropolis.org.uk/assets/07075055.wav')
soundB = S.get-sound-from-url('http://bbcsfx.acropolis.org.uk/assets/07075065.wav')

soundArray = [G.raw-array: soundA, soundB]
#Output is a widget through which concantenated sound can be played, paused or downloaded
r = S.concat(soundArray)








