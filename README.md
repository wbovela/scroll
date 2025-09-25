# scroll
full screen scrolling commodore64 (wip)

Code written in ACME style assembly, specific macro style for C64Studio IDE.
This is work in progress.

This code demonstrates how to do a smooth scroll of an entire screen in character mode, including colour to the left and right.

Currently the code just repeats what falls off one side of the screen and copies it to the other side. 

This code uses macros to use unrolled code for copying data. Unrolled code means that what was previously a classic loop has been written out completely without the use of a loop. This results in faster execution, but more code. Using macros is a way of shortening the notation of these lengthy unrolled loops.

The waitFrame route causes the CPU to be busy waiting until the current raster line is at the bottom of the visible screen. That way changes can be made to the displayed data (at $0400 and $d800 for instance) without the screen being drawn. If the work to be done is more than what the bottom and top borders would allow, the waitFrame routine can be called to divide the work over more than 1 frame. 


