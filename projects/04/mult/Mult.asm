// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[3], respectively.)

// It must not mutate the input values (R0, R1)

// A multiplication (y = n*x) can be represented a sum
// of a value n times (y = sum(x, 1 to n).

// To simplify implementation, here it counts down to zero
// instead of up to n.

	// init R2 (sum) to zero
	@R2
	M=0

	// init the counter (R3) with R1
	@R1		// D[A] = @R1
	D=M		// D = Memory[R1]
	@R3		// D[A] = @R3
	M=D		// Memory[R3] = D

(LOOP)
	// if(n = 0), END
	@R3
	D=M
	@END
	D;JEQ

	// n = n - 1
	@R3
	D=M-1
	@R3
	M=D

	// sum = sum + x
	@R2
	D=M
	@R0
	D=D+M
	@R2
	M=D

	@LOOP
	0;JMP

(END)
	@END
	0;JMP
