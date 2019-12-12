import global as G
import sound as S

# creating a single channel sound
arr1 = [G.raw-array: 0.1, 0.3, -0.3, 0.1, -0.9, 0.2, 1.5] #this is a very short sound!
soundA = S.make-sound(44100, arr1)

#creating a multi-channel sound
arr2 = [G.raw-array: [G.raw-array: 0.1,0.5,0.1], [G.raw-array: -0.1, -0.5, -0.1]]
soundB = S.make-multi-channel-sound(44100, arr2)







