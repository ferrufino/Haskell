tails::[a]->[a]
tails (_:xs) = xs
main = print(tails [5,6,7,8])