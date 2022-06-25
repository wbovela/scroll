# scroll
full screen scrolling commodore64

This code demonstrates how to do a smooth scroll of an entire screen in character mode, including colour. This means moving an entire screen AND its colour ram after the screen has scrolled one whole character. To avoid flickering the routine applies double buffering meaning keeping a working on one half of the screen while the other is being drawn. 
