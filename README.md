# Fly Plane Fly - A Flappy Bird Spin Off
This is a spin off of Flappy Bird written in Haskell.

## Gameplay
![gameplay.gif](https://github.com/jaredponn/Fly-Plane-Fly/blob/master/gameplay.gif)

## Installation

### Prerequisites
This project uses Stack. If you have Stack installed already skip to the next section. 

Note: This has only been tested on Un\*x operating systems.

Run:
```
curl -sSL https://get.haskellstack.org/ | sh
```
or
```
wget -qO- https://get.haskellstack.org/ | sh
```
to install Stack.

Refer to the Stack's [readme](https://docs.haskellstack.org/en/stable/README/) for more details

In addition, this game requires the following SDL2 libraries:
* SDL2
* SDL2 image
* SDL2 mixer
* SDL2 ttf

Installation of these libraries depends on the Linux distribution. In Void Linux, for example, they would be installed by running:
```
sudo xbps-install -S SDL2 SDL2-devel SDL2_image SDL2_image-devel SDL2_mixer SDL2_mixer-devel SDL2_ttf SDL2_ttf-devel 
```
Be sure to download the developer library as well.

### Installing the game
Run:
```
git clone https://github.com/jaredponn/Fly-Plane-Fly.git
cd Fly-Plane-Fly
stack build
stack exec fly-plane-fly
```
And the game should run.

## Implementation details
Please refer to the blog post for an in-depth analysis of the architecture.
TODO

## Project
This project was a high-school project for the Pre-Engineering class. Students may choose to work individually or in groups. Some other projects include Rube Goldberg machines, Quad-copters, and Pancake cookers.

## Issues
The CPU usage is a bit ridiculously high. It takes up more than 50% cpu while running. If you can find the shortcomings in the system that leads to this issue, I would be happy to hear about it.

## Acknowledgments
All the images came from the following sources and were marked free to share use and modify:

 * https://commons.wikimedia.org/wiki/File:P47M_Gerippe.jpg

 * https://www.pexels.com/photo/grey-white-clouds-158163/

 * https://www.publicdomainpictures.net/en/view-image.php?image=111328&picture=large-cumulus-clouds-1

The font [FFFForward](http://www.1001fonts.com/fff-forward-font.html) was used.

The sound effects are from various authors from [freesound](https://freesound.org/)
