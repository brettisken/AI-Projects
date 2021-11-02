Author: Brett Isken
Date: 12/17/2020

Hexapawn - https://en.wikipedia.org/wiki/Hexapawn

This was a project to simulate the game hexapawn and use a neural network to find optimal moves.
MinMax search was used to find optimal moves and board states for both black and white. Then 
a from scratch neural network was used to train a model to pick the best moves. 

Results: 

The best network layout I found was ((10 36) (36 36) (36 9)). That is
3 layers not including the input layer and including the output layer. The
first value of the 2-tuple is the number of input neurons to the layer and 
the second is the number of neurons in that layer. 

The best learning rates were numbers between .2 and .4. Although it did not make
a huge difference. 
I found the sigmoid function performed  best. 

When training the network, I put all optimal policies into an array
and then randomly picked policies and trained. I found doing this over
10,000 times yielded  good results, but 100,000 times yielded  great results. 
From the limited sample of tests I performed after training the network, the network
would always output optimal moves with more than two decimal accuracies. 

Commands:
					Build and Train
(setf layers-list (list (list 10 36) (list 36 36) (list 36 9)))
(setf nn (hexa-train layers-list .3 #'sigmoid #'sigmoid-prime))

					Test
(classify nn (list 0 -1 -1 -1 0 0 0 1 1 1))	
(neural-network-output nn)