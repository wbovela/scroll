# scroll
full screen scrolling commodore64 (wip)

Code written in ACME style assembly. 
This is work in progress.

This code demonstrates how to do a smooth scroll of a partial screen in character mode, including colour to the left and right.

Currently the code just repeats what falls off one side of the screen and copies it to the other side. 

It does not use double buffering, but copies the necessary data during the v-blank period and manages that. Obviously increasing the amount of data by including more lines or colour data will necessitate the work be spread over several frames by calling the waitFrame function at the right time.

This code uses macros to use unrolled code for copying data. Unrolled code means that what was previously a classic loop has been written out completely without the use of a loop. This results in faster execution, but more code. Using macros is a way of shortening the notation of these lengthy unrolled loops.

The waitFrame route causes the CPU to be busy waiting until the current raster line is at the bottom of the visible screen. That way changes can be made to the displayed data (at $0400 and $d800 for instance) without the screen being drawn. If the work to be done is more than what the bottom and top borders would allow, the waitFrame routine can be called to divide the work over more than 1 frame. 

Note that scrolling can be hugely sped up by not soft scrolling every one of the 8 pixels there are. The scrolling speed is so high, that it is hard to notice that the scrolling isn't smooth.


