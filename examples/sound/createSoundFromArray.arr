import global as G
import sound as S

#Getting sound object from URL
#Output is a widget through which sound can be played, paused or downloaded
sound = S.get-sound-from-url('http://bbcsfx.acropolis.org.uk/assets/07075055.wav')


#Getting array from the above sound object
r = S.get-array-from-sound(sound)

#Using the array to create a new sound

#sample rate can be between 3000 to 30000
sampleRate = 3000 

#duration of sound in seconds
duration = 3000 

#number of channels
channels = 2

#Output is a widget through which sound can be played, paused or downloaded
newSound = S.make-sound(channels,sampleRate,duration,r) 







