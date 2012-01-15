#The problem


    Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
    Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.

    Peter and Colin roll their dice and compare totals: the highest total wins. The
    result is a draw if the totals are equal.

    What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer
    rounded to seven decimal places in the form 0.abcdefg


#The approach

We'll take another simulation-based approach. We'll write a function which
simulates `n` fair die rolls of `k` sides, and we'll then use another function
to run a simulation. The latter function will return 1 or 0 depending on if
peter wins or loses. We'll keep track of the total wins for each player and --
asymptotically -- we should see the probability converge to whatever it should
be.
