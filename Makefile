day5:
	stack ghc Day5.hs -- -O -main-is Day5 && cat input_5.txt | ./Day5

day7:
	stack ghc Day7.hs -- -O -main-is Day7 && cat input_7.txt | ./Day7

day7_2_test:
	stack ghc Day7.hs -- -O -main-is Day7 && cat input_7_2_test.txt | ./Day7

day7_2_test_2:
	stack ghc Day7.hs -- -O -main-is Day7 && cat input_7_2_test_2.txt | ./Day7

day8:
	stack ghc Day8.hs -- -O -main-is Day8 && cat input_8.txt | ./Day8

day9:
	stack ghc Day9.hs -- -O -main-is Day9 && cat input_9.txt | ./Day9

day10:
	stack ghc Day10.hs -- -O -main-is Day10 && \
	cat input_10.txt | ./Day10

repl:
	stack exec ghci -- Day10.hs
