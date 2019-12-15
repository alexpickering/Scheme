# Scheme
Scheme code, primarily through DrRacket with BSL and ISL



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


## space-invaders.rkt

### Summary
Simulates the classic arcade game of the same name. The user tries to shoot down invaders and prevent them from reaching the bottom of the screen.

### Description
Start animation with (main 0). The following are functions used within the program, originating with the big-bang function
big-bang
1. move-world - calculates each item's new position by adding speed to current position. New invaders are randomly created, offscreen missiles are deleted, and any colliding missiles and invaders are removed from their respective lists.
* 1.1 remove-collision
* 1.1.1 collect-colliding-missiles
* 1.1.1.1 overlap-missiles
* 1.1.1.1.1 overlap?
* 1.1.2 remove-collision-helper
* 1.2 create-invaders
* 1.3 move-invaders
* 1.3.1 move-invader
* 1.4 next-missiles
* 1.4.1 onscreen-missiles
* 1.4.1.1 onscreen?
* 1.4.2 move-missiles
* 1.4.2.1 move-missile
* 1.5 next-tank
2. render-world (to-draw) - world state is rendered to the screen.
* 2.1 render-invaders
* 2.2 render-missiles
3. end-game? (stop-when) - ends game when an invader reaches the bottom of the screen.
* 3.1 any-reach-bottom?
* 3.1.1 reach-bottom?
4. player-input (on-key) - takes player input to move tank or fire missile
* 4.1 fire-missile
* 4.2 move-tank

### Controls
The following user inputs are accepted to listed effect
- left arrow and right arrow move the user's tank in the respective directions onscreen
- space creates a missile from the tank's current location which travels vertically upwards. If it collides with an invader, both the invader and the missile are destroyed.

