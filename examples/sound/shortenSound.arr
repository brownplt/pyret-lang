import global as G
import sound as S

#Getting sound object from URL
#Output is a widget through which sound can be played, paused or downloaded
sound = S.getSoundFromURL('http://bbcsfx.acropolis.org.uk/assets/07075055.wav')

#Output is a widget through which shortened sound can be played, paused or downloaded
r = S.shorten(sound,0.01,0.04)








