# Scheme
Scheme code, primarily through DrRacket with BSL



## the-office-dvd-animation.rkt

### Summary
This program simulates the well-known scene from The Office (U.S.) during which a DVD logo bounces around on an otherwise blank tv screen.

### Description
Per code comments, start animation with (main (make-dvd x y dx dy cc)) where:
- x and y are in screen coordinates (pixels)
- dx and dy are pixels per tick in the x and y direction
- cc is the current color of the logo
Ex. (main (make-dvd CTR-X CTR-Y 3 -3 "blue")) starts DVD logo at center (from CTR-X, CTR-Y), moving towards the top-right corner (from 3,-3), with the inital color blue (from "blue").

### Controls
Mid-animation, the user can manipulate the location of the dvd logo by:
- pressing the space bar to reset logo position to center screen
- left-clicking within the animation space to change logo's coordinates to that location (unless the logo will not fit on screen where selected)



## sink_blob.rkt

### Summary
This program simulates a single instant of percolation of heavier "solid" blobs towards the bottom of a test tube they share with lighter "bubble" blobs.

### Description
Per code comments, start animation with (sink lob) where:
- ListOfBlob contains a list of elements in their initial state
- Blob is always one of "solid" or "bubble"
- (sink lob) returns the list after solids have sunk 1 place (if bubbles are present further in the list)



## making-rain-lists.rkt

### Summary
This program simulates falling rain, with each new drop created at the position of the mouse click.

### Description
Per code comments, start animation with (main empty) where:
- empty refers to the starting ListOfDrops
- Drops are structs containing x and y coordinates


### Controls
The user can create new drops of rain by left-clicking within the animation space
