import global as G
import sound as S

#Getting sound object from URL
#Output is a widget through which sound can be played, paused or downloaded
buffer = S.getBufferFromURL('http://bbcsfx.acropolis.org.uk/assets/07075055.wav')


#Deormalizing Sound. By default, every sound is normalized. If you do not want sound to be normalized use below function.
t = S.denormalizeSound(buffer)