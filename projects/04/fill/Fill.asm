// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

// Keyboard, @KBD, @24576  (0x6000)
//	0 when no key pressed
//  16-bit ASCII code when pressed

// Screen
//	256 rows, 512 pixels per row (131072)
//  8k memory map, @16384 (0x4000), @SCREEN
//  row: 32 16-bit words (512)
//   M=1  black
//	 M=0  white

// @R1  pixel color
// @R2  counter
// @R3  pixel position

(LOOP)

(CHECK_KEYS)
	// if (key == 0) white else black
	@KBD
	D=M
	@WHITE
	D;JEQ  // if (D == 0) -> WHITE
	@BLACK
	0;JMP

(BLACK)
	@R1  // pixel color
	M=-1  // all 16 pixels black
	@START_DRAW
	0;JMP

(WHITE)
	@R1
	M=0  // all white
	@START_DRAW
	0;JMP

(START_DRAW)
	@8192  // counter (32*256, each 16-bits)
	D=A
	@R2  // counter
	M=D
	@SCREEN
	D=A
	@R3   // pixel position
	M=D
	@DRAW
	0;JMP

(DRAW)
	// if (count == 0) -> END
	@R2
	D=M
	@CHECK_KEYS
	D;JEQ

	// set pixel color
	@R1  // pixel color
	D=M	
	@R3  // pixel position
	A=M
	M=D

	// next pixel position
	@R3
	M=M+1

	// decrement counter
	@R2
	D=M
	M=D-1
	
	// loop
	@DRAW
	0;JMP

// should never get here
(END)
	@END
	0;JMP
