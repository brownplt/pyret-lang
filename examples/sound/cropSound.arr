import global as G
import sound as S

#Getting sound object from URL
#Output is a widget through which sound can be played, paused or downloaded
sound = S.getSoundFromURL('http://bbcsfx.acropolis.org.uk/assets/07075055.wav')

#Crop the sound between certain times
soundB = S.crop-by-time(sound,0.01,0.04)

#crop the sound between two indices
soundC = S.crop-by-index(sound, 0, 100)

#crop off the last n samples of a sound
n = 10
soundD = S.crop-by-index(sound, S.sound-num-samples(sound) - n - 1, S.sound-num-samples(sound))

#crop to only keep the first n samples
soundE = S.crop-by-index(sound, 0, n)









